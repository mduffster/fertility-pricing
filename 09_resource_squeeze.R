# Testing the Resource Squeeze Hypothesis
# The key question: Is it urbanization PLUS resource constraint that matters?

library(tidyverse)
library(fixest)
library(modelsummary)

# Load data
data <- readRDS("data/processed/full_analysis_data.rds")

# Prepare panel
panel_data <- data %>%
  group_by(iso3c) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  filter(year >= 2000, year <= 2020) %>%
  filter(!is.na(fertility_rate))

cat("Panel data:", n_distinct(panel_data$iso3c), "countries,", 
    nrow(panel_data), "observations\n\n")

# ============================================
# CREATE BETTER SQUEEZE MEASURES
# ============================================

panel_data <- panel_data %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(
    # 1. INTERACTION: Urban × Income Constraint
    # High urban + low income = maximum squeeze
    urban_income_interaction = urbanization_rate * (1 / log(gdp_pc_ppp + 1)),
    
    # 2. GROWTH RATE GAP: Urbanizing faster than enriching
    urban_growth_5yr = (urbanization_rate - lag(urbanization_rate, 5)) / 5,
    gdp_growth_5yr = (gdp_pc_ppp - lag(gdp_pc_ppp, 5)) / lag(gdp_pc_ppp, 5) * 100 / 5,
    growth_squeeze = urban_growth_5yr - scale(gdp_growth_5yr)[,1],
    
    # 3. CATEGORICAL: High squeeze = urban but poor
    high_squeeze = (urbanization_rate > median(urbanization_rate, na.rm=T)) & 
      (gdp_pc_ppp < median(gdp_pc_ppp, na.rm=T)),
    
    # 4. RESIDUAL: More urban than income predicts (run regression first)
    # We'll calculate this after
    
  ) %>%
  ungroup()

# Calculate "excess urbanization" (residual method)
# Need to use same data for prediction
urban_model_data <- panel_data %>%
  filter(!is.na(urbanization_rate), !is.na(gdp_pc_ppp), 
         !is.na(female_secondary_enroll))

urban_model <- lm(urbanization_rate ~ log(gdp_pc_ppp) + female_secondary_enroll, 
                  data = urban_model_data)

# Add residuals back to the matching rows
panel_data <- panel_data %>%
  mutate(
    row_id = row_number(),
    excess_urbanization = NA_real_
  )

# Get residuals for rows that were in the model
urban_model_data$excess_urban_calc <- residuals(urban_model)
panel_data <- panel_data %>%
  left_join(
    urban_model_data %>% select(iso3c, year, excess_urban_calc),
    by = c("iso3c", "year")
  ) %>%
  mutate(
    excess_urbanization = coalesce(excess_urbanization, excess_urban_calc)
  ) %>%
  select(-excess_urban_calc, -row_id)

# Standardize key variables for interpretation
panel_data <- panel_data %>%
  mutate(
    urban_z = scale(urbanization_rate)[,1],
    gdp_z = scale(log(gdp_pc_ppp))[,1],
    education_z = scale(female_secondary_enroll)[,1]
  )

# ============================================
# MODEL COMPARISONS
# ============================================

cat("=== TESTING DIFFERENT SQUEEZE SPECIFICATIONS ===\n\n")

# Baseline: Simple urbanization (your original finding)
m1_baseline <- feols(
  fertility_rate ~ urban_z + gdp_z + education_z | iso3c,
  data = panel_data,
  vcov = "hetero"
)

# Test 1: INTERACTION - Urban × Income
# Theory: Effect should be stronger when income is low
m2_interaction <- feols(
  fertility_rate ~ urban_z * gdp_z + education_z | iso3c,
  data = panel_data,
  vcov = "hetero"
)

# Test 2: GROWTH SQUEEZE - Urbanizing faster than enriching
m3_growth_squeeze <- feols(
  fertility_rate ~ growth_squeeze + gdp_z + education_z | iso3c,
  data = panel_data %>% filter(!is.na(growth_squeeze)),
  vcov = "hetero"
)

# Test 3: CATEGORICAL - High squeeze dummy
m4_categorical <- feols(
  fertility_rate ~ high_squeeze + urban_z + gdp_z + education_z | iso3c,
  data = panel_data,
  vcov = "hetero"
)

# Test 4: EXCESS URBANIZATION - More urban than expected
m5_excess <- feols(
  fertility_rate ~ excess_urbanization + gdp_z + education_z | iso3c,
  data = panel_data,
  vcov = "hetero"
)

# Test 5: OECD HOUSING DATA - Direct measure
if (sum(!is.na(panel_data$housing_burden_avg)) > 100) {
  m6_housing <- feols(
    fertility_rate ~ housing_burden_avg + gdp_z + education_z | iso3c,
    data = panel_data %>% filter(!is.na(housing_burden_avg)),
    vcov = "hetero"
  )
} else {
  m6_housing <- NULL
  cat("Note: Insufficient housing data for separate model\n")
}

# ============================================
# RESULTS TABLE
# ============================================

cat("\n=== MODEL COMPARISON ===\n\n")

models_list <- list(
  "Baseline\n(Urban)" = m1_baseline,
  "Interaction\n(Urban×Income)" = m2_interaction,
  "Growth Squeeze" = m3_growth_squeeze,
  "High Squeeze\n(Dummy)" = m4_categorical,
  "Excess Urban" = m5_excess
)

if (!is.null(m6_housing)) {
  models_list[["Housing Burden\n(OECD)"]] = m6_housing
}

# Print summary
modelsummary(
  models_list,
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs", "FE: iso3c"),
  output = "markdown"
)

# ============================================
# KEY INTERPRETATION
# ============================================

cat("\n\n=== INTERPRETATION ===\n\n")

# Extract key coefficients
coef_baseline <- coef(m1_baseline)["urban_z"]
coef_interaction <- coef(m2_interaction)["urban_z:gdp_z"]
coef_growth <- coef(m3_growth_squeeze)["growth_squeeze"]

cat("1. BASELINE URBANIZATION EFFECT:\n")
cat("   Coefficient:", round(coef_baseline, 4), "\n")
cat("   Interpretation: 1 SD increase in urbanization → ", 
    round(coef_baseline, 3), "change in fertility\n\n")

cat("2. INTERACTION TEST (KEY FOR YOUR THEORY):\n")
cat("   Urban × Income coefficient:", round(coef_interaction, 4), "\n")
if (coef_interaction > 0) {
  cat("   ✓ POSITIVE interaction = Urban effect WEAKER at higher income\n")
  cat("   ✓ This supports SQUEEZE hypothesis:\n")
  cat("     - Urbanization hurts more when you're poor\n")
  cat("     - Rich urban countries can afford the bundle\n")
} else {
  cat("   ✗ Negative interaction = Urban effect STRONGER at higher income\n")
  cat("   ✗ This contradicts squeeze hypothesis\n")
}

cat("\n3. GROWTH SQUEEZE (Urbanizing faster than enriching):\n")
cat("   Coefficient:", round(coef_growth, 4), "\n")
if (coef_growth < 0) {
  cat("   ✓ NEGATIVE = When urban growth outpaces GDP, fertility falls\n")
  cat("   ✓ This supports your mechanism!\n")
} else {
  cat("   ✗ Not as expected\n")
}

# ============================================
# VISUALIZATIONS
# ============================================

cat("\n=== Creating visualizations ===\n")

# Plot 1: Interaction effect
if (exists("m2_interaction")) {
  
  # Create predictions at different income levels
  pred_data <- expand.grid(
    urban_z = seq(-2, 2, 0.1),
    gdp_z = c(-1, 0, 1),  # Low, medium, high income
    education_z = 0
  )
  
  # Note: predictions with fixed effects need country reference
  # For visualization, we'll show marginal effects instead
  
  p1 <- panel_data %>%
    filter(!is.na(urban_z), !is.na(gdp_z)) %>%
    mutate(income_tercile = ntile(gdp_pc_ppp, 3)) %>%
    ggplot(aes(x = urbanization_rate, y = fertility_rate, 
               color = factor(income_tercile))) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE) +
    scale_color_manual(
      values = c("red", "orange", "blue"),
      labels = c("Low Income", "Middle Income", "High Income")
    ) +
    labs(
      title = "Testing the Squeeze Hypothesis",
      subtitle = "Does urbanization effect vary by income level?",
      x = "Urbanization Rate (%)",
      y = "Total Fertility Rate",
      color = "Income Level"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("output/figures/squeeze_hypothesis_test.png", p1,
         width = 10, height = 7, dpi = 300)
  cat("Saved: squeeze_hypothesis_test.png\n")
}

# Plot 2: Growth squeeze
if (!all(is.na(panel_data$growth_squeeze))) {
  p2 <- panel_data %>%
    filter(!is.na(growth_squeeze), !is.na(fertility_rate)) %>%
    ggplot(aes(x = growth_squeeze, y = fertility_rate)) +
    geom_point(alpha = 0.3) +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    labs(
      title = "Growth Squeeze Effect",
      subtitle = "When urbanization outpaces GDP growth",
      x = "Growth Squeeze (Urban growth - GDP growth, standardized)",
      y = "Total Fertility Rate"
    ) +
    theme_minimal()
  
  ggsave("output/figures/growth_squeeze.png", p2,
         width = 10, height = 7, dpi = 300)
  cat("Saved: growth_squeeze.png\n")
}

# Save results
save(m1_baseline, m2_interaction, m3_growth_squeeze, 
     m4_categorical, m5_excess, m6_housing,
     file = "output/squeeze_models.RData")

cat("\n=== Analysis complete ===\n")
cat("Check output/figures/ for visualizations\n")
cat("Models saved to output/squeeze_models.RData\n")
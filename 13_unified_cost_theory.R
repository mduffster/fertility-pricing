# 10_mediation_analysis.R
# Testing Whether Cost Measures Explain Urbanization's Effect on Fertility
# Mediation Framework: Does urbanization reduce fertility THROUGH cost constraints?

library(tidyverse)
library(fixest)
library(modelsummary)

cat("=== MEDIATION ANALYSIS: COSTS AS URBANIZATION MECHANISM ===\n\n")
cat("Question: Do cost measures explain WHY urbanization reduces fertility?\n")
cat("Method: If adding costs reduces urbanization's coefficient,\n")
cat("        costs are mediating (explaining) the urbanization effect.\n\n")

# ============================================
# LOAD DATA
# ============================================

cat("Loading data...\n")

# Main dataset with all measures
full_data <- readRDS("data/processed/full_analysis_with_cost_squeeze.rds")

# ============================================
# PREPARE PANEL
# ============================================

panel_data <- full_data %>%
  group_by(iso3c) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  filter(
    year >= 2000, year <= 2020,
    !is.na(fertility_rate)
  ) %>%
  mutate(
    # Standardize all variables
    urban_z = scale(urbanization_rate)[,1],
    gdp_z = scale(log(gdp_pc_ppp))[,1],
    education_z = scale(female_secondary_enroll)[,1],
    
    # Cost measures - check if ANY non-NA values exist, not just if column exists
    housing_z = if(sum(!is.na(housing_burden_avg)) > 1) {
      as.numeric(scale(housing_burden_avg))
    } else {
      NA_real_
    },
    
    health_squeeze_z = if(sum(!is.na(health_squeeze)) > 1) {
      as.numeric(scale(health_squeeze))
    } else {
      NA_real_
    },
    
    squeeze_z = if(sum(!is.na(resource_squeeze_no_housing)) > 1) {
      as.numeric(scale(resource_squeeze_no_housing))
    } else {
      NA_real_
    },
    
    # Income group
    income_group = ntile(gdp_pc_ppp, 3),
    income_group_label = case_when(
      income_group == 1 ~ "Low Income",
      income_group == 2 ~ "Middle Income",
      income_group == 3 ~ "High Income"
    )
  )


# After creating panel_data, add diagnostic section:

cat("\n=== DATA AVAILABILITY CHECK ===\n")
cat("Total observations:", nrow(panel_data), "\n")
cat("\nCost variable coverage:\n")
cat("  Housing burden:    ", sum(!is.na(panel_data$housing_z)), "obs\n")
cat("  Health OOP:        ", sum(!is.na(panel_data$health_squeeze_z)), "obs\n")
cat("  Squeeze index:     ", sum(!is.na(panel_data$squeeze_z)), "obs\n")
cat("  Urbanization:      ", sum(!is.na(panel_data$urban_z)), "obs\n")

# Check original variables before standardization
if ("housing_burden_avg" %in% names(panel_data)) {
  cat("\nOriginal housing_burden_avg:", sum(!is.na(panel_data$housing_burden_avg)), "obs\n")
}
if ("health_oop_pc" %in% names(panel_data)) {
  cat("Original health_oop_pc:", sum(!is.na(panel_data$health_oop_pc)), "obs\n")
}
if ("resource_squeeze_no_housing" %in% names(panel_data)) {
  cat("Original resource_squeeze_no_housing:", sum(!is.na(panel_data$resource_squeeze_no_housing)), "obs\n")
}

cat("\n")

# After the data loading section, add:
cat("\nAvailable cost variables:\n")
cat("  Housing burden:", "housing_burden_avg" %in% names(full_data), "\n")
cat("  Health OOP:", "health_oop_pc" %in% names(full_data), "\n")
cat("  Squeeze index:", "resource_squeeze_no_housing" %in% names(full_data), "\n")

# ============================================
# MEDIATION ANALYSIS: ALL COUNTRIES
# ============================================

cat("=== MEDIATION ANALYSIS (ALL COUNTRIES) ===\n\n")

# Step 1: Total effect (baseline)
m1_total <- feols(
  fertility_rate ~ urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data,
  vcov = "hetero"
)

# Step 2: Add health costs (only if data exists)
if (sum(!is.na(panel_data$health_squeeze_z)) > 100) {
  m2_health <- feols(
    fertility_rate ~ urban_z + health_squeeze_z + gdp_z + education_z | iso3c + year,
    data = panel_data %>% filter(!is.na(health_squeeze_z)),
    vcov = "hetero"
  )
} else {
  cat("Insufficient health cost data - skipping this model\n")
  m2_health <- NULL
}

# Step 3: Add squeeze index (only if data exists)
if (sum(!is.na(panel_data$squeeze_z)) > 100) {
  m3_squeeze <- feols(
    fertility_rate ~ urban_z + squeeze_z + gdp_z + education_z | iso3c + year,
    data = panel_data %>% filter(!is.na(squeeze_z)),
    vcov = "hetero"
  )
} else {
  cat("Insufficient squeeze data - skipping this model\n")
  m3_squeeze <- NULL
}

# Step 4: Multiple costs (only if both exist)
if (!is.null(m2_health) && !is.null(m3_squeeze)) {
  m4_multiple <- feols(
    fertility_rate ~ urban_z + health_squeeze_z + squeeze_z + gdp_z + education_z | iso3c + year,
    data = panel_data %>% filter(!is.na(health_squeeze_z), !is.na(squeeze_z)),
    vcov = "hetero"
  )
} else {
  m4_multiple <- NULL
}

# Only include non-NULL models in the list
mediation_models <- list(
  "Baseline\n(Total Effect)" = m1_total
)

if (!is.null(m2_health)) mediation_models[["Add Health\nCosts"]] <- m2_health
if (!is.null(m3_squeeze)) mediation_models[["Add Squeeze\nIndex"]] <- m3_squeeze
if (!is.null(m4_multiple)) mediation_models[["Multiple\nCosts"]] <- m4_multiple

# Only run modelsummary if we have at least 2 models
if (length(mediation_models) >= 2) {
  modelsummary(
    mediation_models,
    stars = TRUE,
    gof_map = c("r.squared", "adj.r.squared", "nobs"),
    output = "markdown"
  )
} else {
  cat("Insufficient models for comparison table\n")
  print(summary(m1_total))
}

# ============================================
# MEDIATION ANALYSIS: HIGH-INCOME (WITH HOUSING)
# ============================================

cat("\n\n=== MEDIATION ANALYSIS (HIGH-INCOME COUNTRIES) ===\n")
cat("These countries have direct housing cost data\n\n")

high_income_data <- panel_data %>%
  filter(
    income_group_label == "High Income",
    year >= 2010, year <= 2019  # Match housing data availability
  )

# Step 1: Total effect
m5_hi_total <- feols(
  fertility_rate ~ urban_z + gdp_z + education_z | iso3c + year,
  data = high_income_data,
  vcov = "hetero"
)

# Step 2: Add housing burden
m6_hi_housing <- feols(
  fertility_rate ~ urban_z + housing_z + gdp_z + education_z | iso3c + year,
  data = high_income_data %>% filter(!is.na(housing_z)),
  vcov = "hetero"
)

# Step 3: Add health costs
m7_hi_health <- feols(
  fertility_rate ~ urban_z + housing_z + health_squeeze_z + gdp_z + education_z | iso3c + year,
  data = high_income_data %>% filter(!is.na(housing_z), !is.na(health_squeeze_z)),
  vcov = "hetero"
)

# Step 4: Full cost model
m8_hi_full <- feols(
  fertility_rate ~ urban_z + housing_z + health_squeeze_z + squeeze_z + 
    gdp_z + education_z | iso3c + year,
  data = high_income_data %>% filter(!is.na(housing_z), !is.na(health_squeeze_z), 
                                     !is.na(squeeze_z)),
  vcov = "hetero"
)

high_income_models <- list(
  "Baseline" = m5_hi_total
)

if (!is.null(m6_hi_housing)) high_income_models[["Add Housing"]] <- m6_hi_housing
if (!is.null(m7_hi_health)) high_income_models[["Add Health"]] <- m7_hi_health
if (!is.null(m8_hi_full)) high_income_models[["Full Costs"]] <- m8_hi_full

if (length(high_income_models) >= 2) {
  modelsummary(
    high_income_models,
    stars = TRUE,
    gof_map = c("r.squared", "adj.r.squared", "nobs"),
    output = "markdown"
  )
} else {
  cat("Insufficient high-income models\n")
}

# ============================================
# CALCULATE MEDIATION EFFECTS
# ============================================

cat("\n\n=== MEDIATION SUMMARY ===\n\n")

# All countries mediation
urban_total <- coef(m1_total)["urban_z"]
urban_with_costs <- coef(m4_multiple)["urban_z"]
pct_mediated_all <- (urban_total - urban_with_costs) / urban_total * 100

cat("ALL COUNTRIES:\n")
cat(sprintf("  Total urbanization effect:     %.4f\n", urban_total))
cat(sprintf("  Urbanization effect with costs: %.4f\n", urban_with_costs))
cat(sprintf("  Reduction in coefficient:       %.4f (%.1f%%)\n", 
            urban_total - urban_with_costs, pct_mediated_all))

if (pct_mediated_all > 0) {
  cat("  ✓ Cost measures partially explain urbanization's effect\n")
} else {
  cat("  ✗ Cost measures don't reduce urbanization coefficient\n")
}

# High-income mediation
if (exists("m5_hi_total") && exists("m8_hi_full")) {
  urban_hi_total <- coef(m5_hi_total)["urban_z"]
  urban_hi_costs <- coef(m8_hi_full)["urban_z"]
  pct_mediated_hi <- (urban_hi_total - urban_hi_costs) / urban_hi_total * 100
  
  cat("\nHIGH-INCOME COUNTRIES:\n")
  cat(sprintf("  Total urbanization effect:     %.4f\n", urban_hi_total))
  cat(sprintf("  Urbanization effect with costs: %.4f\n", urban_hi_costs))
  cat(sprintf("  Reduction in coefficient:       %.4f (%.1f%%)\n", 
              urban_hi_total - urban_hi_costs, pct_mediated_hi))
  
  if (pct_mediated_hi > 20) {
    cat("  ✓✓ Strong mediation: costs explain substantial portion\n")
  } else if (pct_mediated_hi > 0) {
    cat("  ✓ Partial mediation: costs explain some of the effect\n")
  }
}

# ============================================
# INDIVIDUAL COST COMPONENTS
# ============================================

cat("\n\n=== WHICH COSTS MATTER MOST? ===\n\n")

# Test each cost component individually
if (!is.na(coef(m2_health)["health_squeeze_z"])) {
  cat(sprintf("Health out-of-pocket:  %.4f\n", coef(m2_health)["health_squeeze_z"]))
}

if (!is.na(coef(m3_squeeze)["squeeze_z"])) {
  cat(sprintf("Resource squeeze:      %.4f\n", coef(m3_squeeze)["squeeze_z"]))
}

if (exists("m6_hi_housing") && "housing_z" %in% names(coef(m6_hi_housing))) {
  cat(sprintf("Housing burden (HI):   %.4f\n", coef(m6_hi_housing)["housing_z"]))
}

# ============================================
# VISUALIZATIONS
# ============================================

cat("\n\nCreating visualizations...\n")

# Plot 1: Mediation effect sizes
mediation_summary <- data.frame(
  Model = c("Baseline", "Add Health", "Add Squeeze", "Multiple Costs"),
  Urban_Coef = c(
    coef(m1_total)["urban_z"],
    coef(m2_health)["urban_z"],
    coef(m3_squeeze)["urban_z"],
    coef(m4_multiple)["urban_z"]
  ),
  N = c(nobs(m1_total), nobs(m2_health), nobs(m3_squeeze), nobs(m4_multiple))
)

p1 <- ggplot(mediation_summary, aes(x = Model, y = Urban_Coef)) +
  geom_col(fill = "steelblue") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Mediation Analysis: Do Costs Explain Urbanization's Effect?",
    subtitle = "If bars shrink toward zero, costs are mediating the effect",
    y = "Urbanization Coefficient",
    x = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave("output/figures/mediation_urbanization.png", p1,
       width = 10, height = 7, dpi = 300)

# Plot 2: High-income mediation (if housing data available)
if (exists("m5_hi_total")) {
  hi_summary <- data.frame(
    Model = c("Baseline", "Add Housing", "Add Health", "Full Costs"),
    Urban_Coef = c(
      coef(m5_hi_total)["urban_z"],
      coef(m6_hi_housing)["urban_z"],
      coef(m7_hi_health)["urban_z"],
      coef(m8_hi_full)["urban_z"]
    )
  )
  
  p2 <- ggplot(hi_summary, aes(x = Model, y = Urban_Coef)) +
    geom_col(fill = "darkgreen") +
    geom_hline(yintercept = 0, linetype = "dashed") +
    labs(
      title = "High-Income Countries: Mediation with Direct Cost Measures",
      subtitle = "Including actual housing burden data",
      y = "Urbanization Coefficient",
      x = NULL
    ) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  
  ggsave("output/figures/mediation_high_income.png", p2,
         width = 10, height = 7, dpi = 300)
}

# ============================================
# SAVE RESULTS
# ============================================

save(m1_total, m2_health, m3_squeeze, m4_multiple,
     m5_hi_total, m6_hi_housing, m7_hi_health, m8_hi_full,
     file = "output/mediation_models.RData")

modelsummary(
  mediation_models,
  stars = TRUE,
  output = "output/tables/mediation_analysis.html",
  title = "Mediation Analysis: Do Costs Explain Urbanization?"
)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Key question: Did urbanization coefficient shrink when we added costs?\n")
cat("If yes → costs explain the mechanism\n")
cat("If no → need to rethink measurement or mechanism\n")
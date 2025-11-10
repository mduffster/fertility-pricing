# 14_lagged_housing_mediation.R
# Testing: Does urbanization → housing costs → fertility (with proper lags)?
# Focus: Level-based measures with temporal ordering

library(tidyverse)
library(fixest)
library(modelsummary)

cat("=== LAGGED HOUSING MEDIATION ANALYSIS ===\n\n")
cat("Theory: Urbanization increases housing costs, which reduces fertility\n")
cat("Approach: Test with proper temporal ordering and level measures\n\n")

# ============================================
# LOAD DATA & CREATE LAGS
# ============================================

full_data <- readRDS("data/processed/full_analysis_with_cost_squeeze.rds")

# Create lagged variables
data_with_lags <- full_data %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(
    # Lags for temporal ordering
    urban_lag3 = lag(urbanization_rate, 3),
    urban_lag5 = lag(urbanization_rate, 5),
    housing_lag2 = lag(housing_burden_avg, 2),
    
    # Standardize
    urban_z = scale(urbanization_rate)[,1],
    urban_lag3_z = scale(urban_lag3)[,1],
    urban_lag5_z = scale(urban_lag5)[,1],
    housing_z = scale(housing_burden_avg)[,1],
    housing_lag2_z = scale(housing_lag2)[,1],
    gdp_z = scale(log(gdp_pc_ppp))[,1],
    education_z = scale(female_secondary_enroll)[,1]
  ) %>%
  ungroup()

# ============================================
# HOUSING PANEL (2010-2019, OECD countries)
# ============================================

housing_panel <- data_with_lags %>%
  filter(
    !is.na(housing_burden_avg),
    !is.na(fertility_rate),
    year >= 2010, year <= 2019
  ) %>%
  group_by(iso3c) %>%
  filter(n() >= 5) %>%  # At least 5 years of data
  ungroup()

cat("Housing panel:\n")
cat("  Countries:", n_distinct(housing_panel$iso3c), "\n")
cat("  Observations:", nrow(housing_panel), "\n")
cat("  Years:", min(housing_panel$year), "to", max(housing_panel$year), "\n\n")

# ============================================
# PATH A: Does urbanization predict housing costs?
# ============================================

cat("=== PATH A: Urbanization → Housing Costs ===\n\n")

# Contemporaneous
path_a1 <- feols(
  housing_z ~ urban_z + gdp_z + education_z | iso3c + year,
  data = housing_panel,
  vcov = "hetero"
)

# Lagged urbanization
path_a2 <- feols(
  housing_z ~ urban_lag3_z + gdp_z + education_z | iso3c + year,
  data = housing_panel %>% filter(!is.na(urban_lag3_z)),
  vcov = "hetero"
)

path_a3 <- feols(
  housing_z ~ urban_lag5_z + gdp_z + education_z | iso3c + year,
  data = housing_panel %>% filter(!is.na(urban_lag5_z)),
  vcov = "hetero"
)

modelsummary(
  list(
    "Contemp." = path_a1,
    "Lag 3yr" = path_a2,
    "Lag 5yr" = path_a3
  ),
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  output = "markdown"
)

# ============================================
# PATH B: Does housing predict fertility?
# ============================================

cat("\n\n=== PATH B: Housing Costs → Fertility ===\n\n")

# Contemporaneous
path_b1 <- feols(
  fertility_rate ~ housing_z + gdp_z + education_z | iso3c + year,
  data = housing_panel,
  vcov = "hetero"
)

# Lagged housing
path_b2 <- feols(
  fertility_rate ~ housing_lag2_z + gdp_z + education_z | iso3c + year,
  data = housing_panel %>% filter(!is.na(housing_lag2_z)),
  vcov = "hetero"
)

modelsummary(
  list(
    "Contemp." = path_b1,
    "Lag 2yr" = path_b2
  ),
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  output = "markdown"
)

# ============================================
# FULL MEDIATION: Urbanization → Housing → Fertility
# ============================================

cat("\n\n=== MEDIATION TEST ===\n\n")

m1_total <- feols(
  fertility_rate ~ urban_lag5_z + gdp_z + education_z | iso3c,
  data = housing_panel %>% filter(!is.na(urban_lag5_z)),
  vcov = "hetero"
)

m2_mediated <- feols(
  fertility_rate ~ urban_lag5_z + housing_z + gdp_z + education_z | iso3c,
  data = housing_panel %>% filter(!is.na(urban_lag5_z)),
  vcov = "hetero"
)

modelsummary(
  list(
    "Total Effect" = m1_total,
    "With Housing" = m2_mediated
  ),
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  output = "markdown"
)

# ============================================
# RESULTS SUMMARY
# ============================================

cat("\n\n=== MEDIATION SUMMARY ===\n\n")

urban_total <- coef(m1_total)["urban_lag5_z"]
urban_mediated <- coef(m2_mediated)["urban_lag5_z"]
housing_effect <- coef(m2_mediated)["housing_z"]

pct_mediated <- (urban_total - urban_mediated) / urban_total * 100

cat(sprintf("Total effect (urbanization → fertility):      %.4f\n", urban_total))
cat(sprintf("Direct effect (controlling for housing):      %.4f\n", urban_mediated))
cat(sprintf("Housing coefficient:                          %.4f\n", housing_effect))
cat(sprintf("\nMediation: %.1f%% of urbanization's effect\n", pct_mediated))

if (pct_mediated > 20 && pct_mediated < 100) {
  cat("✓ Partial mediation: Housing costs explain part of urbanization's effect\n")
} else if (pct_mediated >= 100) {
  cat("✓✓ Full mediation: Housing costs fully explain urbanization's effect\n")
} else if (pct_mediated > 0) {
  cat("✓ Weak mediation: Small portion explained by housing\n")
} else {
  cat("✗ No mediation: Housing doesn't explain urbanization's mechanism\n")
}

# Path A × Path B (indirect effect)
path_a_coef <- coef(path_a2)["urban_lag3_z"]
path_b_coef <- coef(path_b1)["housing_z"]
indirect <- path_a_coef * path_b_coef

cat(sprintf("\nIndirect effect (Path A × Path B):            %.4f\n", indirect))
cat(sprintf("  (Urban→Housing: %.4f) × (Housing→Fertility: %.4f)\n", 
            path_a_coef, path_b_coef))

# ============================================
# VISUALIZATION
# ============================================

cat("\n\nCreating visualization...\n")

mediation_plot <- data.frame(
  Model = c("Total Effect\n(Urban→Fertility)", 
            "Direct Effect\n(+ Housing Control)"),
  Coefficient = c(urban_total, urban_mediated)
)

p <- ggplot(mediation_plot, aes(x = Model, y = Coefficient)) +
  geom_col(fill = c("steelblue", "darkgreen"), width = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  labs(
    title = "Housing Cost Mediation (OECD Countries, 2010-2019)",
    subtitle = sprintf("%.1f%% of urbanization's effect explained by housing costs", 
                       pct_mediated),
    y = "Urbanization Coefficient",
    x = NULL
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 11))

ggsave("output/figures/housing_mediation_lagged.png", p,
       width = 8, height = 6, dpi = 300)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Limited to OECD countries 2010-2019 with housing data\n")
cat("Results show whether housing costs mediate urbanization's effect\n")
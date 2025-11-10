# 09_urbanization_mechanism_lagged.R
# Urbanization as Core Resource Constraint with Lagged Effects
# Tests labor share effects with appropriate time lags

library(tidyverse)
library(fixest)
library(modelsummary)
library(pwt10)

cat("=== URBANIZATION MECHANISM WITH LAGGED LABOR SHARE ===\n\n")
cat("Key insight: Fertility decisions lag economic conditions\n")
cat("Testing 3, 5, and 7-year lags in labor share effects\n\n")

# ============================================
# STEP 1: LOAD AND PREPARE DATA
# ============================================

cat("Step 1: Loading Penn World Tables data...\n")

data("pwt10.0")

pwt_clean <- pwt10.0 %>%
  select(country, isocode, year, rgdpna, rkna, emp, avh, labsh, ctfp, csh_i) %>%
  filter(year >= 1990) %>%
  mutate(
    capital_per_worker = rkna / emp,
    labor_share_pct = labsh * 100,
    total_labor = emp * avh,
    kl_ratio = rkna / total_labor
  ) %>%
  rename(iso3c = isocode)

cat("   PWT data:", nrow(pwt_clean), "observations\n\n")

# Load fertility data
fertility_data <- readRDS("data/processed/full_analysis_with_cost_squeeze.rds")

# Merge
analysis_data <- fertility_data %>%
  left_join(pwt_clean, by = c("iso3c", "year"))

cat("Step 2: Creating lagged variables...\n")

# Create all lags and dynamic measures
analysis_data <- analysis_data %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(
    # Lagged labor share (levels)
    labor_share_lag3 = lag(labor_share_pct, 3),
    labor_share_lag5 = lag(labor_share_pct, 5),
    labor_share_lag7 = lag(labor_share_pct, 7),
    
    # Changes in labor share
    labor_share_change_3yr = labor_share_pct - lag(labor_share_pct, 3),
    labor_share_change_5yr = labor_share_pct - lag(labor_share_pct, 5),
    labor_share_change_7yr = labor_share_pct - lag(labor_share_pct, 7),
    
    # Long difference: 2000 vs 2020
    year_2000 = (year == 2000),
    year_2020 = (year == 2020),
    
    # Standardized versions
    kl_z = scale(log(capital_per_worker))[,1],
    labor_share_z = scale(labor_share_pct)[,1],
    labor_share_lag3_z = scale(labor_share_lag3)[,1],
    labor_share_lag5_z = scale(labor_share_lag5)[,1],
    labor_share_lag7_z = scale(labor_share_lag7)[,1]
  ) %>%
  ungroup()

# ============================================
# STEP 3: PREPARE PANEL
# ============================================

panel_data <- analysis_data %>%
  group_by(iso3c) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  filter(
    year >= 2000, year <= 2020,
    !is.na(fertility_rate),
    !is.na(capital_per_worker)
  ) %>%
  mutate(
    urban_z = scale(urbanization_rate)[,1],
    gdp_z = scale(log(gdp_pc_ppp))[,1],
    education_z = scale(female_secondary_enroll)[,1],
    income_group = ntile(gdp_pc_ppp, 3),
    income_group_label = case_when(
      income_group == 1 ~ "Low Income",
      income_group == 2 ~ "Middle Income", 
      income_group == 3 ~ "High Income"
    )
  )

cat("   Panel data:", nrow(panel_data), "observations\n")
cat("   Countries:", n_distinct(panel_data$iso3c), "\n\n")

# ============================================
# MODELS: TESTING LAG STRUCTURES
# ============================================

cat("=== TESTING LAGGED LABOR SHARE EFFECTS ===\n\n")

# Baseline: contemporaneous
m1_contemp <- feols(
  fertility_rate ~ labor_share_z + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data,
  vcov = "hetero"
)

# 3-year lag
m2_lag3 <- feols(
  fertility_rate ~ labor_share_lag3_z + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(!is.na(labor_share_lag3_z)),
  vcov = "hetero"
)

# 5-year lag
m3_lag5 <- feols(
  fertility_rate ~ labor_share_lag5_z + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(!is.na(labor_share_lag5_z)),
  vcov = "hetero"
)

# 7-year lag
m4_lag7 <- feols(
  fertility_rate ~ labor_share_lag7_z + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(!is.na(labor_share_lag7_z)),
  vcov = "hetero"
)

# Distributed lag model (cumulative effect)
m5_distributed <- feols(
  fertility_rate ~ labor_share_z + labor_share_lag3_z + labor_share_lag5_z + 
    urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(!is.na(labor_share_lag5_z)),
  vcov = "hetero"
)

# Changes specification (first differences approach)
m6_changes <- feols(
  fertility_rate ~ labor_share_change_5yr + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(!is.na(labor_share_change_5yr)),
  vcov = "hetero"
)

cat("\n=== LAG COMPARISON TABLE ===\n\n")

lag_models <- list(
  "Contemporaneous" = m1_contemp,
  "3-Year Lag" = m2_lag3,
  "5-Year Lag" = m3_lag5,
  "7-Year Lag" = m4_lag7,
  "Distributed Lag" = m5_distributed,
  "5-Yr Change" = m6_changes
)

modelsummary(
  lag_models,
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs", "FE: iso3c", "FE: year"),
  output = "markdown"
)

# ============================================
# LAGGED EFFECTS BY INCOME GROUP
# ============================================

cat("\n=== LAGGED EFFECTS BY INCOME GROUP ===\n\n")

# High-income countries with 5-year lag
m7_high_income <- feols(
  fertility_rate ~ labor_share_lag5_z + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(income_group_label == "High Income", 
                               !is.na(labor_share_lag5_z)),
  vcov = "hetero"
)

# Split by income group
m8_by_income_lagged <- feols(
  fertility_rate ~ labor_share_lag5_z + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(!is.na(labor_share_lag5_z)),
  split = ~income_group_label,
  vcov = "hetero"
)

etable(m8_by_income_lagged)

# ============================================
# INTERACTION: URBAN × LAGGED LABOR SHARE
# ============================================

cat("\n=== URBANIZATION × LAGGED LABOR SHARE ===\n\n")

m9_interaction_lag5 <- feols(
  fertility_rate ~ urban_z * labor_share_lag5_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(!is.na(labor_share_lag5_z)),
  vcov = "hetero"
)

# By income group
m10_interaction_high_income <- feols(
  fertility_rate ~ urban_z * labor_share_lag5_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(income_group_label == "High Income", 
                               !is.na(labor_share_lag5_z)),
  vcov = "hetero"
)

interaction_models <- list(
  "All Countries" = m9_interaction_lag5,
  "High Income Only" = m10_interaction_high_income
)

modelsummary(
  interaction_models,
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  output = "markdown"
)

# ============================================
# LONG DIFFERENCES (2000 vs 2020)
# ============================================

# cat("\n=== LONG DIFFERENCES APPROACH ===\n\n")
# 
# # Calculate changes over full period
# long_diff_data <- analysis_data %>%
#   filter(year %in% c(2000, 2020), !is.na(fertility_rate), !is.na(labor_share_pct)) %>%
#   group_by(iso3c) %>%
#   filter(n() == 2) %>%
#   arrange(year) %>%
#   summarise(
#     fertility_change = fertility_rate[2] - fertility_rate[1],
#     labor_share_change = labor_share_pct[2] - labor_share_pct[1],
#     urban_change = urbanization_rate[2] - urbanization_rate[1],
#     gdp_2000 = gdp_pc_ppp[1],
#     .groups = "drop"
#   ) %>%
#   mutate(
#     # Create income groups based on 2000 GDP
#     income_group = ntile(gdp_2000, 3),
#     income_group_label = case_when(
#       income_group == 1 ~ "Low Income",
#       income_group == 2 ~ "Middle Income",
#       income_group == 3 ~ "High Income"
#     )
#   )
# 
# # Long difference regression
# m11_long_diff <- lm(
#   fertility_change ~ labor_share_change + urban_change + log(gdp_2000),
#   data = long_diff_data
# )
# 
# # By income group
# m12_long_diff_high <- lm(
#   fertility_change ~ labor_share_change + urban_change + log(gdp_2000),
#   data = long_diff_data %>% filter(income_group == "High Income")
# )
# 
# cat("\nLong Differences (2000-2020):\n")
# summary(m11_long_diff)
# 
# cat("\nHigh-Income Countries Only:\n")
# summary(m12_long_diff_high)

# ============================================
# INTERPRETATION
# ============================================

cat("\n\n=== KEY FINDINGS ===\n\n")

# Extract coefficients for comparison
coef_contemp <- coef(m1_contemp)["labor_share_z"]
coef_lag3 <- coef(m2_lag3)["labor_share_lag3_z"]
coef_lag5 <- coef(m3_lag5)["labor_share_lag5_z"]
coef_lag7 <- coef(m4_lag7)["labor_share_lag7_z"]

cat("1. OPTIMAL LAG STRUCTURE:\n")
cat(sprintf("   Contemporaneous: %.4f\n", coef_contemp))
cat(sprintf("   3-year lag:      %.4f\n", coef_lag3))
cat(sprintf("   5-year lag:      %.4f\n", coef_lag5))
cat(sprintf("   7-year lag:      %.4f\n", coef_lag7))

# Determine best lag
coefs <- c(contemp = coef_contemp, lag3 = coef_lag3, 
           lag5 = coef_lag5, lag7 = coef_lag7)
best_lag <- names(which.max(abs(coefs)))

cat(sprintf("\n   Strongest effect at: %s\n", best_lag))

if (abs(coef_lag5) > abs(coef_contemp)) {
  cat("   ✓ Lagged effects are stronger than contemporaneous\n")
  cat("   ✓ Supports hypothesis that fertility lags economic conditions\n")
}

cat("\n2. INCOME GROUP HETEROGENEITY (5-year lag):\n")
if (exists("m8_by_income_lagged")) {
  # Extract coefficients from split model properly
  if ("High Income" %in% names(m8_by_income_lagged)) {
    high_model <- m8_by_income_lagged$`High Income`
    if ("labor_share_lag5_z" %in% names(coef(high_model))) {
      high_coef <- coef(high_model)["labor_share_lag5_z"]
      cat(sprintf("   High-income countries: %.4f\n", high_coef))
      
      if (!is.na(high_coef) && high_coef > 0) {
        cat("   ✓ Labor share from 5 years ago predicts current fertility\n")
        cat("   ✓ Young workers' early career conditions affect family formation\n")
      }
    } else {
      cat("   Labor share coefficient not found in high-income model\n")
    }
  }
}

cat("\n3. URBANIZATION × LABOR SHARE INTERACTION:\n")
if ("urban_z:labor_share_lag5_z" %in% names(coef(m9_interaction_lag5))) {
  int_coef <- coef(m9_interaction_lag5)["urban_z:labor_share_lag5_z"]
  int_pval <- summary(m9_interaction_lag5)$coeftable["urban_z:labor_share_lag5_z", "Pr(>|t|)"]
  
  cat(sprintf("   Urban × Labor Share(t-5): %.4f (p = %.4f)\n", int_coef, int_pval))
  
  if (int_coef > 0 & int_pval < 0.05) {
    cat("   ✓ Urban squeeze is worse when past labor share was low\n")
    cat("   ✓ Early career conditions affect ability to afford urban life\n")
  }
}

# cat("\n4. LONG-RUN CHANGES (2000-2020):\n")
# if (exists("m11_long_diff")) {
#   ls_change_coef <- coef(m11_long_diff)["labor_share_change"]
#   cat(sprintf("   Labor share change coefficient: %.4f\n", ls_change_coef))
#   
#   if (ls_change_coef > 0) {
#     cat("   ✓ Countries with declining labor share show larger fertility drops\n")
#   }
# }

# ============================================
# SAVE OUTPUTS
# ============================================

# Save models
save(m1_contemp, m2_lag3, m3_lag5, m4_lag7, m5_distributed, m6_changes,
     m7_high_income, m8_by_income_lagged, m9_interaction_lag5, 
     m10_interaction_high_income, m11_long_diff,
     file = "output/urbanization_lagged_models.RData")

# Save summary table
modelsummary(
  lag_models,
  stars = TRUE,
  output = "output/tables/lagged_labor_share_results.html",
  title = "Labor Share Effects with Time Lags"
)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Models saved to output/urbanization_lagged_models.RData\n")
cat("Tables saved to output/tables/\n")
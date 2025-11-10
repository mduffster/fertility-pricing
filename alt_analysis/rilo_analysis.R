library(tidyverse)
library(WDI)
library(fixest)
library(modelsummary)

cat("=== QUICK TEST: EMPLOYMENT STRUCTURE & FERTILITY ===\n\n")

# Download World Bank employment data
cat("Step 1: Downloading employment data from World Bank...\n")
emp_wb <- WDI(
  indicator = c(
    "SL.EMP.SELF.ZS",      # Self-employed, total (% of total employment)
    "SL.AGR.EMPL.ZS",      # Employment in agriculture (% of total employment)
    "SL.IND.EMPL.ZS",      # Employment in industry (% of total employment)
    "SL.SRV.EMPL.ZS"       # Employment in services (% of total employment)
  ),
  start = 1990,
  end = 2023,
  extra = TRUE
)

# Clean
emp_clean <- emp_wb %>%
  filter(region != "Aggregates") %>%
  rename(
    self_employment_pct = SL.EMP.SELF.ZS,
    agriculture_employment_pct = SL.AGR.EMPL.ZS,
    industry_employment_pct = SL.IND.EMPL.ZS,
    services_employment_pct = SL.SRV.EMPL.ZS
  ) %>%
  select(iso3c, country, year, 
         self_employment_pct, agriculture_employment_pct,
         industry_employment_pct, services_employment_pct)

cat("  Downloaded:", nrow(emp_clean), "observations\n")
cat("  Countries:", n_distinct(emp_clean$iso3c), "\n")
cat("  Self-employment data:", sum(!is.na(emp_clean$self_employment_pct)), "obs\n\n")

# Step 2: Merge with fertility data
cat("Step 2: Merging with fertility data...\n")
full_data <- readRDS("data/processed/full_analysis_data.rds") %>%
  left_join(emp_clean, by = c("iso3c", "year"))

# Step 3: Prepare panel
cat("Step 3: Preparing panel data...\n")
panel_data <- full_data %>%
  group_by(iso3c) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  filter(
    year >= 2000, year <= 2020,
    !is.na(fertility_rate),
    !is.na(self_employment_pct)
  ) %>%
  mutate(
    # Standardize variables
    urban_z = scale(urbanization_rate)[,1],
    gdp_z = scale(log(gdp_pc_ppp))[,1],
    education_z = scale(female_secondary_enroll)[,1],
    self_emp_z = scale(self_employment_pct)[,1],
    ag_emp_z = scale(agriculture_employment_pct)[,1],
    
    # Non-agricultural self-employment (rough proxy for business owners vs farmers)
    nonag_self_emp_pct = self_employment_pct * (1 - agriculture_employment_pct/100),
    nonag_self_emp_z = scale(nonag_self_emp_pct)[,1]
  )

cat("  Panel observations:", nrow(panel_data), "\n")
cat("  Countries:", n_distinct(panel_data$iso3c), "\n\n")

# Step 4: Run tests
cat("=== RUNNING REGRESSIONS ===\n\n")

# Model 1: Baseline (no employment structure)
m1_baseline <- feols(
  fertility_rate ~ urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data,
  vcov = "hetero"
)

# Model 2: Add self-employment
m2_self_emp <- feols(
  fertility_rate ~ self_emp_z + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data,
  vcov = "hetero"
)

# Model 3: Control for agriculture
m3_with_ag <- feols(
  fertility_rate ~ self_emp_z + ag_emp_z + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data,
  vcov = "hetero"
)

# Model 4: Non-agricultural self-employment (business owners, not farmers)
m4_nonag <- feols(
  fertility_rate ~ nonag_self_emp_z + ag_emp_z + urban_z + gdp_z + education_z | iso3c + year,
  data = panel_data %>% filter(!is.na(nonag_self_emp_z)),
  vcov = "hetero"
)

# Model 5: Interaction - does self-employment buffer urbanization?
m5_interaction <- feols(
  fertility_rate ~ self_emp_z * urban_z + ag_emp_z + gdp_z + education_z | iso3c + year,
  data = panel_data,
  vcov = "hetero"
)

# Results table
cat("=== RESULTS ===\n\n")
modelsummary(
  list(
    "Baseline" = m1_baseline,
    "Add Self-Emp" = m2_self_emp,
    "Control Ag" = m3_with_ag,
    "Non-Ag Self" = m4_nonag,
    "Interaction" = m5_interaction
  ),
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  output = "markdown"
)

# Interpretation
cat("\n\n=== INTERPRETATION ===\n\n")

self_emp_coef <- coef(m2_self_emp)["self_emp_z"]
self_emp_pval <- summary(m2_self_emp)$coeftable["self_emp_z", "Pr(>|t|)"]

cat(sprintf("Self-employment coefficient: %.4f (p = %.3f)\n\n", self_emp_coef, self_emp_pval))

if (self_emp_coef > 0.1 && self_emp_pval < 0.05) {
  cat("✓✓ STRONG EVIDENCE: Self-employment predicts HIGHER fertility\n")
  cat("   → Labor structure matters independent of income/urbanization\n")
  cat("   → Wage work creates squeeze, self-employment provides buffer\n\n")
  
  if ("self_emp_z:urban_z" %in% names(coef(m5_interaction))) {
    int_coef <- coef(m5_interaction)["self_emp_z:urban_z"]
    cat(sprintf("   Interaction (Self-emp × Urban): %.4f\n", int_coef))
    if (int_coef > 0) {
      cat("   → Self-employment BUFFERS the urban fertility penalty\n")
    }
  }
  
  cat("\n   YOU'RE ONTO SOMETHING BIG.\n")
  cat("   Next steps:\n")
  cat("   - Get microdata to test within-urban variation\n")
  cat("   - Look for natural experiments (policy changes affecting employment)\n")
  cat("   - Test mechanism: schedule flexibility, income volatility\n")
  
} else if (self_emp_coef > 0 && self_emp_pval < 0.1) {
  cat("✓ SUGGESTIVE EVIDENCE: Weakly positive\n")
  cat("   → May need more refined measures\n")
  cat("   → Consider: informal vs. formal, employers vs. own-account\n")
  
} else if (self_emp_coef < 0) {
  cat("✗ OPPOSITE DIRECTION: Self-employment predicts LOWER fertility\n")
  cat("   → This contradicts the labor structure hypothesis\n")
  cat("   → Possible confound: self-employment correlated with poverty in some countries\n")
  
} else {
  cat("○ NO CLEAR EFFECT: Self-employment doesn't predict fertility\n")
  cat("   → Labor structure may not be the key mechanism (at least not captured this way)\n")
  cat("   → Consider alternative explanations or better measures\n")
}

# Save models
save(m1_baseline, m2_self_emp, m3_with_ag, m4_nonag, m5_interaction,
     file = "output/employment_structure_models.RData")

cat("\n=== Models saved to output/employment_structure_models.RData ===\n")
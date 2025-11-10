# Create housing + utilities burden measure and test against fertility

analysis_data <- analysis_data %>%
  mutate(
    # Calculate total monthly costs
    monthly_rent = ifelse(RENT > 0, RENT, 0),
    monthly_utilities = (COSTELEC + COSTGAS) / 12,  # Annual to monthly
    
    # Total housing + utilities burden as % of monthly income
    monthly_income = HHINCOME / 12,
    total_burden = (monthly_rent + monthly_utilities) / (monthly_income + 1),
    
    # Categorize burden levels
    burden_category = case_when(
      total_burden <= 0.3 ~ "Affordable (<30%)",
      total_burden > 0.3 & total_burden <= 0.5 ~ "Burdened (30-50%)",
      total_burden > 0.5 ~ "Severely burdened (>50%)",
      TRUE ~ NA_character_
    ),
    
    high_burden = (total_burden > 0.3)
  )

# Check the distribution
cat("=== COST BURDEN DISTRIBUTION ===\n")
table(analysis_data$burden_category, useNA = "ifany")

cat("\nMean burden by employment type:\n")
analysis_data %>%
  group_by(employment_type) %>%
  summarise(
    mean_burden = mean(total_burden, na.rm = TRUE),
    median_burden = median(total_burden, na.rm = TRUE),
    n = n()
  ) %>%
  print()

# Model 1: Does cost burden predict fertility?
m_burden <- feols(
  children_in_hh ~ factor(burden_category) + metro + 
    factor(employment_type) + AGE + I(AGE^2) + 
    factor(education) + log(HHINCOME + 1) | YEAR,
  data = analysis_data %>% filter(!is.na(burden_category)),
  weights = ~PERWT,
  vcov = "hetero"
)

# Model 2: Does burden interact with employment?
m_burden_emp <- feols(
  children_in_hh ~ factor(burden_category) * factor(employment_type) + 
    metro + AGE + I(AGE^2) + factor(education) + 
    log(HHINCOME + 1) | YEAR,
  data = analysis_data %>% filter(!is.na(burden_category)),
  weights = ~PERWT,
  vcov = "hetero"
)

# Model 3: By income group
m_burden_income <- feols(
  children_in_hh ~ factor(burden_category) + metro + 
    factor(employment_type) + AGE + I(AGE^2) + 
    factor(education) | YEAR,
  data = analysis_data %>% filter(!is.na(burden_category)),
  split = ~income_group,
  weights = ~PERWT,
  vcov = "hetero"
)

# Results
cat("\n=== MODEL 1: Cost Burden Effect ===\n")
modelsummary(
  list("Cost Burden" = m_burden),
  stars = TRUE,
  gof_map = c("nobs", "r2")
)

cat("\n=== MODEL 2: Burden × Employment ===\n")
modelsummary(
  list("Burden×Employment" = m_burden_emp),
  stars = TRUE,
  gof_map = c("nobs", "r2")
)

cat("\n=== MODEL 3: By Income Level ===\n")
etable(m_burden_income, digits = 3)

# Save
save(m_burden, m_burden_emp, m_burden_income, analysis_data,
     file = "output/ipums_cost_burden_results.RData")

cat("\nResults saved to output/ipums_cost_burden_results.RData\n")
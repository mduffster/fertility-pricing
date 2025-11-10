# Create income groups
analysis_data <- analysis_data %>%
  mutate(
    income_quintile = ntile(HHINCOME, 5),
    income_group = case_when(
      income_quintile <= 2 ~ "Low income (Q1-Q2)",
      income_quintile == 3 ~ "Middle income (Q3)",
      income_quintile >= 4 ~ "High income (Q4-Q5)"
    )
  )

# Run model split by income group
m_by_income <- feols(
  children_in_hh ~ metro * factor(employment_type) + 
    AGE + I(AGE^2) + factor(education) | YEAR,
  data = analysis_data,
  split = ~income_group,
  weights = ~PERWT,
  vcov = "hetero"
)

# Display all results
modelsummary(
  m_by_income,
  stars = TRUE,
  gof_map = c("nobs", "r2")
)

# Or extract specific coefficients
cat("\n=== EMPLOYMENT EFFECTS BY INCOME LEVEL ===\n\n")

# The models are indexed 1, 2, 3 corresponding to the factor levels
# Check the names first
cat("Available splits:", names(m_by_income), "\n\n")

# Extract coefficients from each model
for (i in 1:length(m_by_income)) {
  cat(names(m_by_income)[i], ":\n")
  model <- m_by_income[[i]]
  
  self_coef <- coef(model)["factor(employment_type)Self-employed"]
  wage_coef <- coef(model)["factor(employment_type)Wage worker"]
  metro_self <- coef(model)["metroTRUE:factor(employment_type)Self-employed"]
  
  cat("  Self-employed:", round(self_coef, 3), "\n")
  cat("  Wage worker:", round(wage_coef, 3), "\n")
  cat("  Metro × Self-employed:", round(metro_self, 3), "\n\n")
}
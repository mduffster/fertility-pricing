# Build Minimum Viable Bundle Cost Index
# Combines: housing, food, transport, healthcare, education, utilities

library(tidyverse)

cat("=== BUILDING MINIMUM VIABLE BUNDLE COST INDEX ===\n\n")

# Load cost components
cost_data <- readRDS("data/processed/cost_components.rds")

# Load housing data (already have this)
housing_data <- readRDS("data/processed/oecd_housing.rds")

# Load main analysis data for fertility and other vars
main_data <- readRDS("data/processed/full_analysis_data.rds")

# ============================================
# STEP 1: CALCULATE GROWTH RATES (NOT LEVELS)
# ============================================

cat("Step 1: Calculating growth rates for costs and income...\n")

# Calculate changes over time for each country
cost_normalized <- cost_data %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(
    # INCOME GROWTH (denominator)
    income_growth_5yr = (gdp_pc_ppp - lag(gdp_pc_ppp, 5)) / lag(gdp_pc_ppp, 5) * 100 / 5,
    
    # COST CHANGES (numerators)
    # 1. Food: inflation is already a rate of change
    food_inflation_rate = pmax(inflation, 0, na.rm = FALSE),
    
    # 2. Healthcare: growth in out-of-pocket costs per capita
    health_cost_lag = lag(health_oop_pc, 5),
    health_cost_growth = ifelse(!is.na(health_oop_pc) & !is.na(health_cost_lag) & health_cost_lag > 0,
                                (health_oop_pc - health_cost_lag) / health_cost_lag * 100 / 5,
                                NA_real_),
    
    # 3. Housing: change in burden (will add from OECD)
    
    # 4. Education: change in expenditure as % of GDP
    edu_expend_lag = lag(edu_expend_gdp, 5),
    edu_cost_change = edu_expend_gdp - edu_expend_lag,
    
    # 5. Utilities: growth in electricity consumption (proxy for utility cost growth)
    electric_lag = lag(electricity_consumption_pc, 5),
    utility_growth = ifelse(!is.na(electricity_consumption_pc) & !is.na(electric_lag) & electric_lag > 0,
                            (electricity_consumption_pc - electric_lag) / electric_lag * 100 / 5,
                            NA_real_)
  ) %>%
  ungroup()

cat("   Checking growth rate variables:\n")
cat("   - Income growth: ", sum(!is.na(cost_normalized$income_growth_5yr)), " non-NA\n")
cat("   - Food inflation: ", sum(!is.na(cost_normalized$food_inflation_rate)), " non-NA\n")
cat("   - Health cost growth: ", sum(!is.na(cost_normalized$health_cost_growth)), " non-NA\n")
cat("   - Education change: ", sum(!is.na(cost_normalized$edu_cost_change)), " non-NA\n")

# ============================================
# STEP 2: ADD HOUSING GROWTH DATA
# ============================================

cat("\nStep 2: Adding housing burden changes...\n")

# Load and calculate housing burden changes
if (file.exists("data/processed/oecd_housing.rds")) {
  housing_data <- readRDS("data/processed/oecd_housing.rds") %>%
    group_by(iso3c) %>%
    arrange(year) %>%
    mutate(
      housing_burden_change = housing_burden_avg - lag(housing_burden_avg, 5)
    ) %>%
    ungroup()
  
  cost_with_housing <- cost_normalized %>%
    left_join(
      housing_data %>% select(iso3c, year, housing_burden_avg, housing_burden_change),
      by = c("iso3c", "year")
    )
  
  cat("   - Countries with housing data:", 
      n_distinct(cost_with_housing$iso3c[!is.na(cost_with_housing$housing_burden_avg)]), "\n")
  cat("   - Housing burden changes: ", 
      sum(!is.na(cost_with_housing$housing_burden_change)), " non-NA\n")
} else {
  cost_with_housing <- cost_normalized
}

# ============================================
# STEP 3: CREATE SQUEEZE MEASURES (GROWTH DIFFERENTIALS)
# ============================================

cat("\nStep 3: Creating resource squeeze measures...\n")

# Weights for combining different cost components
WEIGHTS <- list(
  housing = 0.35,
  food = 0.30,
  health = 0.20,
  education = 0.15
)

cat("\nUsing weights:\n")
print(unlist(WEIGHTS))

cost_with_housing <- cost_with_housing %>%
  group_by(iso3c) %>%
  mutate(
    # SQUEEZE = Cost growth - Income growth
    
    # 1. Food squeeze: inflation above income growth
    food_squeeze = food_inflation_rate - income_growth_5yr,
    
    # 2. Health squeeze: health cost growth above income growth  
    health_squeeze = health_cost_growth - income_growth_5yr,
    
    # 3. Housing squeeze: housing burden increase (already relative measure)
    housing_squeeze = housing_burden_change,
    
    # 4. Education squeeze: education cost increase
    education_squeeze = edu_cost_change,
    
    # COMPOSITE SQUEEZE INDEX
    # With housing (OECD countries)
    resource_squeeze_full = (
      WEIGHTS$housing * housing_squeeze +
        WEIGHTS$food * food_squeeze +
        WEIGHTS$health * health_squeeze +
        WEIGHTS$education * education_squeeze
    ) / sum(unlist(WEIGHTS)),
    
    # Without housing (broader coverage)
    resource_squeeze_no_housing = (
      WEIGHTS$food * food_squeeze +
        WEIGHTS$health * health_squeeze +
        WEIGHTS$education * education_squeeze
    ) / (WEIGHTS$food + WEIGHTS$health + WEIGHTS$education),
    
    # Flag which have full data
    has_housing_data = !is.na(housing_burden_avg),
    has_squeeze_data = !is.na(resource_squeeze_no_housing)
    
  ) %>%
  ungroup()

# Check coverage
cat("\nSqueeze measure coverage:\n")
cat("   - Food squeeze: ", sum(!is.na(cost_with_housing$food_squeeze)), " obs\n")
cat("   - Health squeeze: ", sum(!is.na(cost_with_housing$health_squeeze)), " obs\n")
cat("   - Housing squeeze: ", sum(!is.na(cost_with_housing$housing_squeeze)), " obs\n")
cat("   - Full squeeze index: ", sum(!is.na(cost_with_housing$resource_squeeze_full)), " obs\n")
cat("   - Partial squeeze index: ", sum(!is.na(cost_with_housing$resource_squeeze_no_housing)), " obs\n")

# ============================================
# STEP 4: VALIDATE SQUEEZE MEASURES
# ============================================

cat("\nStep 4: Validating squeeze measures...\n")

# Check correlations with known measures
validation <- cost_with_housing %>%
  filter(year >= 2015) %>%
  select(resource_squeeze_full, resource_squeeze_no_housing,
         food_squeeze, health_squeeze, housing_squeeze,
         income_growth_5yr, gdp_pc_ppp) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

cat("\nCorrelations:\n")
print(validation)

# Summary statistics  
cat("\n=== SQUEEZE INDEX SUMMARY (2015-2020) ===\n")
summary_recent <- cost_with_housing %>%
  filter(year >= 2015) %>%
  summarise(
    n_obs = n(),
    n_with_full_squeeze = sum(!is.na(resource_squeeze_full)),
    n_with_partial_squeeze = sum(!is.na(resource_squeeze_no_housing)),
    mean_squeeze = mean(resource_squeeze_no_housing, na.rm = TRUE),
    sd_squeeze = sd(resource_squeeze_no_housing, na.rm = TRUE),
    min_squeeze = min(resource_squeeze_no_housing, na.rm = TRUE),
    max_squeeze = max(resource_squeeze_no_housing, na.rm = TRUE)
  )

print(summary_recent)

# ============================================
# STEP 5: MERGE WITH MAIN ANALYSIS DATA
# ============================================

cat("\nStep 5: Merging with main fertility dataset...\n")

# Merge the squeeze measures into main analysis data
main_with_costs <- main_data %>%
  left_join(
    cost_with_housing %>% 
      select(iso3c, year, 
             resource_squeeze_full,
             resource_squeeze_no_housing,
             food_squeeze, health_squeeze, housing_squeeze, education_squeeze,
             income_growth_5yr,
             has_housing_data, has_squeeze_data),
    by = c("iso3c", "year")
  )

cat("   - Final dataset:", nrow(main_with_costs), "observations\n")
cat("   - Countries:", n_distinct(main_with_costs$iso3c), "\n")
cat("   - With full squeeze index:", sum(!is.na(main_with_costs$resource_squeeze_full)), "\n")
cat("   - With partial squeeze index:", sum(!is.na(main_with_costs$resource_squeeze_no_housing)), "\n")

# ============================================
# STEP 6: SAVE EVERYTHING
# ============================================

cat("\nStep 6: Saving datasets...\n")

saveRDS(cost_with_housing, "data/processed/cost_squeeze_detailed.rds")
saveRDS(main_with_costs, "data/processed/full_analysis_with_cost_squeeze.rds")
write_csv(main_with_costs, "data/processed/full_analysis_with_cost_squeeze.csv")

# Save just the squeeze measures for easy reference
squeeze_only <- cost_with_housing %>%
  select(iso3c, country, year, region,
         resource_squeeze_full,
         resource_squeeze_no_housing,
         food_squeeze, health_squeeze, housing_squeeze, education_squeeze,
         income_growth_5yr,
         has_housing_data, has_squeeze_data,
         gdp_pc_ppp)

write_csv(squeeze_only, "data/processed/resource_squeeze_index.csv")

cat("\n=== RESOURCE SQUEEZE INDEX BUILT SUCCESSFULLY ===\n")
cat("\nFiles saved:\n")
cat("  - data/processed/cost_squeeze_detailed.rds\n")
cat("  - data/processed/full_analysis_with_cost_squeeze.rds\n")
cat("  - data/processed/resource_squeeze_index.csv\n")
cat("\nNext: Update 06_improved_measures.R to use squeeze data, then test with 10_test_cost_index.R\n")
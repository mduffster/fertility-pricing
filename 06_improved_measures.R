# Create All Resource Pressure Measures
# This consolidates basic proxies + improved measures + cost index data
# Run AFTER: cost index is built (step 5)
# Run BEFORE: exploratory analysis (step 7)

library(tidyverse)

cat("=== CREATING COMPREHENSIVE RESOURCE PRESSURE MEASURES ===\n\n")

# ============================================
# LOAD DATA
# ============================================

# Load the dataset with cost index already merged in
full_data <- readRDS("data/processed/full_analysis_with_cost_index.rds")

cat("Loaded data:", nrow(full_data), "observations\n")
cat("Countries:", n_distinct(full_data$iso3c), "\n")
cat("Years:", min(full_data$year, na.rm=TRUE), "to", max(full_data$year, na.rm=TRUE), "\n\n")

# ============================================
# CREATE ALL RESOURCE PRESSURE MEASURES
# ============================================

cat("Creating resource pressure measures...\n")

full_data <- full_data %>%
  group_by(iso3c) %>%
  arrange(year) %>%
  mutate(
    # ===== BASIC PROXIES =====
    
    # 1. Income constraint (inverse of GDP)
    income_constraint = 1 / (gdp_pc_ppp / 1000),
    
    # 2. Simple resource pressure: urbanization + income constraint
    resource_pressure_index = (scale(urbanization_rate)[,1] + 
                                 scale(income_constraint)[,1]) / 2,
    
    # 3. Needs vs capacity gap
    needs_capacity_gap = scale(urbanization_rate)[,1] - scale(log(gdp_pc_ppp))[,1],
    
    
    # ===== IMPROVED DYNAMIC MEASURES =====
    
    # 4. Growth rates (captures squeeze over time)
    urban_growth = (urbanization_rate - lag(urbanization_rate, 5)) / 5,
    gdp_growth_5yr = (gdp_pc_ppp - lag(gdp_pc_ppp, 5)) / lag(gdp_pc_ppp, 5) * 100 / 5,
    
    # 5. Growth squeeze: urbanizing faster than enriching
    growth_squeeze = scale(urban_growth)[,1] - scale(gdp_growth_5yr)[,1],
    
    
    # ===== LEVEL-BASED SQUEEZE MEASURES =====
    
    # 6. Urban-to-income ratio
    urban_to_income_ratio = urbanization_rate / log(gdp_pc_ppp + 1),
    
    # 7. Modernization without wealth
    modernization_score = scale(urbanization_rate)[,1] + 
      scale(female_secondary_enroll)[,1],
    wealth_score = scale(log(gdp_pc_ppp))[,1],
    modern_wealth_gap = modernization_score - wealth_score,
    
    
    # ===== CATEGORICAL SQUEEZE =====
    
    # 8. High squeeze dummy (urban but poor)
    high_squeeze = (urbanization_rate > median(urbanization_rate, na.rm=TRUE)) & 
      (gdp_pc_ppp < median(gdp_pc_ppp, na.rm=TRUE)),
    
    
    # ===== INTERACTION TERMS (for regression) =====
    
    # 9. Urban × Income interaction components
    urban_z = scale(urbanization_rate)[,1],
    gdp_z = scale(log(gdp_pc_ppp))[,1],
    education_z = scale(female_secondary_enroll)[,1],
    
    # For cost index (standardized)
    bundle_index_z = scale(bundle_cost_index_no_housing)[,1],
    squeeze_z = scale(resource_squeeze_no_housing)[,1],
    
    
    # ===== POPULATION DENSITY MEASURES =====
    
    # 10. Population density (people per sq km of agricultural land)
    pop_density_ag = population / (ag_land_pct / 100)
    
  ) %>%
  ungroup()

# ============================================
# CALCULATE EXCESS URBANIZATION (RESIDUAL METHOD)
# ============================================

cat("\nCalculating excess urbanization (residual method)...\n")

# Regress urbanization on GDP and education
urban_model_data <- full_data %>%
  filter(!is.na(urbanization_rate), !is.na(gdp_pc_ppp), 
         !is.na(female_secondary_enroll))

urban_model <- lm(urbanization_rate ~ log(gdp_pc_ppp) + female_secondary_enroll, 
                  data = urban_model_data)

# Add residuals back
urban_model_data$excess_urban_calc <- residuals(urban_model)

full_data <- full_data %>%
  left_join(
    urban_model_data %>% select(iso3c, year, excess_urban_calc),
    by = c("iso3c", "year")
  ) %>%
  mutate(
    excess_urbanization = excess_urban_calc
  ) %>%
  select(-excess_urban_calc)

# ============================================
# MERGE WITH OECD HOUSING DATA IF AVAILABLE
# ============================================

cat("\nMerging OECD housing data...\n")

if (file.exists("data/processed/oecd_housing.rds")) {
  housing_data <- readRDS("data/processed/oecd_housing.rds")
  
  # Check if already merged
  if (!"housing_burden_avg" %in% names(full_data)) {
    full_data <- full_data %>%
      left_join(
        housing_data %>% select(iso3c, year, housing_burden_rent, 
                                housing_burden_mortgage, housing_burden_avg),
        by = c("iso3c", "year")
      )
  }
  
  cat("   Countries with housing data:", 
      n_distinct(full_data$iso3c[!is.na(full_data$housing_burden_avg)]), "\n")
  
  # Create improved resource pressure with housing data
  full_data <- full_data %>%
    mutate(
      # Use actual housing burden if available, otherwise use proxy
      housing_cost_indicator = coalesce(housing_burden_avg, income_constraint * 10),
      
      # Improved resource pressure with real housing data
      resource_pressure_v2 = scale(housing_cost_indicator)[,1] + 
        scale(urbanization_rate)[,1]
    )
}

# ============================================
# SAVE FINAL DATASET
# ============================================

cat("\nSaving final analysis dataset...\n")

saveRDS(full_data, "data/processed/full_analysis_data.rds")
write_csv(full_data, "data/processed/full_analysis_data.csv")

# ============================================
# VALIDATION & SUMMARY
# ============================================

cat("\n=== VALIDATION: Correlations with Fertility (2020) ===\n")

cor_test <- full_data %>%
  filter(year == 2020) %>%
  select(fertility_rate, 
         urban_to_income_ratio,
         modern_wealth_gap,
         growth_squeeze,
         bundle_cost_index_no_housing,
         resource_squeeze_no_housing,
         housing_burden_avg,
         urbanization_rate,
         gdp_pc_ppp,
         female_secondary_enroll) %>%
  cor(use = "pairwise.complete.obs") %>%
  round(3)

cat("\nCorrelations with Fertility Rate:\n")
print(cor_test[, "fertility_rate"])

# For OECD countries with housing data
if (sum(!is.na(full_data$housing_burden_avg)) > 50) {
  cat("\n=== OECD Countries Only (with housing data) ===\n")
  oecd_cor <- full_data %>%
    filter(!is.na(housing_burden_avg), year >= 2015) %>%
    select(fertility_rate, housing_burden_avg, bundle_cost_index_full,
           gdp_pc_ppp, female_secondary_enroll) %>%
    cor(use = "pairwise.complete.obs") %>%
    round(3)
  
  cat("Correlations with fertility:\n")
  print(oecd_cor[, "fertility_rate"])
}

# Quick visualization
if (sum(!is.na(full_data$housing_burden_avg)) > 30) {
  cat("\nCreating housing burden plot...\n")
  
  oecd_recent <- full_data %>%
    filter(!is.na(housing_burden_avg), year == 2020)
  
  p <- ggplot(oecd_recent, aes(x = housing_burden_avg, y = fertility_rate)) +
    geom_point(aes(size = population), alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE) +
    geom_text(aes(label = iso3c), size = 3, vjust = -0.5) +
    labs(
      title = "Housing Cost Burden vs Fertility (OECD Countries)",
      subtitle = "2020 data - Direct measurement of resource pressure",
      x = "Housing Cost Burden (% of income)",
      y = "Total Fertility Rate"
    ) +
    theme_minimal()
  
  dir.create("output/figures", showWarnings = FALSE, recursive = TRUE)
  ggsave("output/figures/housing_burden_fertility.png", p, 
         width = 10, height = 7, dpi = 300)
  cat("   Saved: output/figures/housing_burden_fertility.png\n")
}

# ============================================
# SUMMARY
# ============================================

cat("\n=== SUMMARY ===\n")
cat("Final dataset created with:\n")
cat("  - Basic proxies (urbanization, income constraint)\n")
cat("  - Dynamic measures (growth squeeze)\n")
cat("  - Level measures (urban-income ratio, modern-wealth gap)\n")
cat("  - Cost index measures (bundle cost, resource squeeze)\n")
cat("  - Interaction terms (standardized variables)\n")
cat("  - Excess urbanization (residual method)\n")

cat("\nDataset details:\n")
cat("  Total observations:", nrow(full_data), "\n")
cat("  Countries:", n_distinct(full_data$iso3c), "\n")
cat("  Years:", min(full_data$year, na.rm=TRUE), "to", max(full_data$year, na.rm=TRUE), "\n")

if ("bundle_cost_index_no_housing" %in% names(full_data)) {
  cat("  With cost index:", sum(!is.na(full_data$bundle_cost_index_no_housing)), "obs\n")
}
if ("housing_burden_avg" %in% names(full_data)) {
  cat("  With housing burden:", sum(!is.na(full_data$housing_burden_avg)), "obs\n")
}

cat("\n=== READY FOR ANALYSIS ===\n")
cat("Next steps:\n")
cat("  07_exploratory.R - Visualizations\n")
cat("  08_panel_baseline.R - Basic panel models\n")
cat("  09_resource_squeeze.R - Test squeeze hypothesis\n")
cat("  10_test_cost_index.R - Test cost index vs proxies\n")
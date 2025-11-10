# Test the Resource Squeeze Index (Growth Differentials) Against Fertility
# Compare to urbanization and other proxies

library(tidyverse)
library(fixest)
library(modelsummary)

cat("=== TESTING RESOURCE SQUEEZE INDEX VS FERTILITY ===\n\n")

# Load data with squeeze measures
data <- readRDS("data/processed/full_analysis_with_cost_squeeze.rds")

# Prepare panel
panel_data <- data %>%
  group_by(iso3c) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  filter(year >= 2000, year <= 2020,
         !is.na(fertility_rate))

cat("Panel data:", n_distinct(panel_data$iso3c), "countries\n")
cat("Observations:", nrow(panel_data), "\n\n")

# Standardize key variables
panel_data <- panel_data %>%
  mutate(
    # Standardize for comparability
    squeeze_z = scale(resource_squeeze_no_housing)[,1],
    urban_z = scale(urbanization_rate)[,1],
    gdp_z = scale(log(gdp_pc_ppp))[,1],
    education_z = scale(female_secondary_enroll)[,1]
  )

# ============================================
# MODEL COMPARISONS
# ============================================

cat("=== RUNNING REGRESSIONS ===\n\n")

# Model 1: Baseline - Urbanization (our original proxy)
m1_urban <- feols(
  fertility_rate ~ urban_z + gdp_z + education_z | iso3c,
  data = panel_data,
  vcov = "hetero"
)

# Model 2: Resource Squeeze (growth differential) instead of urbanization
m2_squeeze <- feols(
  fertility_rate ~ squeeze_z + gdp_z + education_z | iso3c,
  data = panel_data %>% filter(!is.na(squeeze_z)),
  vcov = "hetero"
)

# Model 3: Horse race - both urbanization and squeeze
m3_both <- feols(
  fertility_rate ~ squeeze_z + urban_z + gdp_z + education_z | iso3c,
  data = panel_data %>% filter(!is.na(squeeze_z)),
  vcov = "hetero"
)

# Model 4: Component decomposition - which squeezes matter most?
m4_components <- feols(
  fertility_rate ~ food_squeeze + health_squeeze + gdp_z + education_z | iso3c,
  data = panel_data %>% filter(!is.na(food_squeeze) | !is.na(health_squeeze)),
  vcov = "hetero"
)

# Model 5: For OECD countries with full squeeze (including housing)
if (sum(!is.na(panel_data$resource_squeeze_full)) > 100) {
  m5_oecd_full <- feols(
    fertility_rate ~ resource_squeeze_full + gdp_z + education_z | iso3c,
    data = panel_data %>% filter(!is.na(resource_squeeze_full)),
    vcov = "hetero"
  )
} else {
  m5_oecd_full <- NULL
}

# Model 6: Growth squeeze (from earlier script)
if ("growth_squeeze" %in% names(panel_data)) {
  m6_growth <- feols(
    fertility_rate ~ growth_squeeze + gdp_z + education_z | iso3c,
    data = panel_data %>% filter(!is.na(growth_squeeze)),
    vcov = "hetero"
  )
} else {
  m6_growth <- NULL
}

# ============================================
# RESULTS TABLE
# ============================================

cat("=== MODEL COMPARISON ===\n\n")

# Only include models that successfully ran
models <- list(
  "Urbanization\n(proxy)" = m1_urban,
  "Resource Squeeze\n(growth diff)" = m2_squeeze,
  "Both" = m3_both,
  "Components" = m4_components
)

# Add optional models if they exist
if (!is.null(m5_oecd_full) && nobs(m5_oecd_full) > 50) {
  models[["Full Squeeze\n(OECD)"]] = m5_oecd_full
}
if (!is.null(m6_growth) && nobs(m6_growth) > 50) {
  models[["Growth Squeeze\n(urban-gdp)"]] = m6_growth
}

modelsummary(
  models,
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs", "FE: iso3c"),
  output = "markdown"
)

# Save to file
modelsummary(
  models[1:3],  # Main models only
  stars = TRUE,
  output = "output/tables/squeeze_index_results.html",
  title = "Testing Resource Squeeze Index (Growth Differentials)"
)

# ============================================
# KEY COMPARISONS
# ============================================

cat("\n\n=== KEY FINDINGS ===\n\n")

# Extract R-squared values
r2_urban <- r2(m1_urban)["wr2"]
r2_squeeze <- r2(m2_squeeze)["wr2"]
r2_both <- r2(m3_both)["wr2"]

cat("1. EXPLANATORY POWER (Within R²):\n")
cat(sprintf("   Urbanization proxy:   %.4f\n", r2_urban))
cat(sprintf("   Resource Squeeze:     %.4f", r2_squeeze))
if (r2_squeeze > r2_urban) {
  cat(sprintf(" ✓ BETTER (%.4f improvement)\n", r2_squeeze - r2_urban))
} else {
  cat(sprintf(" (%.4f change)\n", r2_squeeze - r2_urban))
}
cat(sprintf("   Both together:        %.4f\n", r2_both))

# Coefficient comparison
cat("\n2. COEFFICIENT MAGNITUDES:\n")
cat(sprintf("   Urbanization:         %.4f\n", coef(m1_urban)["urban_z"]))
cat(sprintf("   Resource Squeeze:     %.4f\n", coef(m2_squeeze)["squeeze_z"]))

# Horse race
cat("\n3. HORSE RACE (both in same model):\n")
if (exists("m3_both")) {
  cat(sprintf("   Resource Squeeze:     %.4f (p = %.4f)\n", 
              coef(m3_both)["squeeze_z"],
              summary(m3_both)$coeftable["squeeze_z", "Pr(>|t|)"]))
  cat(sprintf("   Urbanization:         %.4f (p = %.4f)\n",
              coef(m3_both)["urban_z"],
              summary(m3_both)$coeftable["urban_z", "Pr(>|t|)"]))
  cat(sprintf("   Within R²:            %.4f\n", r2(m3_both)["wr2"]))
  
  squeeze_sig <- summary(m3_both)$coeftable["squeeze_z", "Pr(>|t|)"] < 0.05
  urban_sig <- summary(m3_both)$coeftable["urban_z", "Pr(>|t|)"] < 0.05
  
  if (squeeze_sig && !urban_sig) {
    cat("   → Resource Squeeze dominates!\n")
  } else if (urban_sig && !squeeze_sig) {
    cat("   → Urbanization is the stronger predictor\n")
  } else if (squeeze_sig && urban_sig) {
    cat("   → Both are significant - independent effects\n")
  }
}

# Component analysis
cat("\n4. WHICH COST COMPONENTS MATTER MOST?\n")
if (exists("m4_components")) {
  components <- c("food_squeeze", "health_squeeze")
  for (comp in components) {
    if (comp %in% names(coef(m4_components))) {
      pval <- summary(m4_components)$coeftable[comp, "Pr(>|t|)"]
      sig <- if (pval < 0.001) "***" else if (pval < 0.01) "**" else if (pval < 0.05) "*" else ""
      cat(sprintf("   %s: %.4f %s\n", 
                  gsub("_squeeze", "", comp),
                  coef(m4_components)[comp],
                  sig))
    }
  }
}

# ============================================
# VISUALIZATION
# ============================================

cat("\n=== Creating visualizations ===\n")

# Plot: Resource Squeeze vs Fertility
plot_data <- panel_data %>%
  filter(!is.na(resource_squeeze_no_housing), !is.na(fertility_rate))

if (nrow(plot_data) > 100) {
  # Take recent data
  recent <- plot_data %>% filter(year >= 2015)
  
  if (nrow(recent) > 50) {
    p1 <- ggplot(recent, aes(x = resource_squeeze_no_housing, y = fertility_rate)) +
      geom_point(aes(size = population, color = region), alpha = 0.6) +
      geom_smooth(method = "lm", se = TRUE, color = "darkred") +
      labs(
        title = "Resource Squeeze Index vs Fertility",
        subtitle = "Growth differential: (cost growth - income growth)",
        x = "Resource Squeeze Index (higher = costs rising faster than income)",
        y = "Total Fertility Rate",
        caption = "Combines food inflation, health cost growth, education costs"
      ) +
      theme_minimal() +
      theme(legend.position = "bottom")
    
    ggsave("output/figures/squeeze_index_vs_fertility.png", p1,
           width = 10, height = 7, dpi = 300)
    cat("Saved: squeeze_index_vs_fertility.png\n")
  }
}

# Plot 2: Model comparison
comparison_df <- data.frame(
  Model = c("Urbanization", "Resource Squeeze", "Both Together"),
  Within_R2 = c(r2_urban, r2_squeeze, r2_both),
  N = c(nobs(m1_urban), nobs(m2_squeeze), nobs(m3_both))
)

p2 <- ggplot(comparison_df, aes(x = Model, y = Within_R2)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.3f", Within_R2)), vjust = -0.5) +
  ylim(0, max(comparison_df$Within_R2) * 1.1) +
  labs(
    title = "Model Comparison: Squeeze Index vs Urbanization",
    subtitle = "Within-country R² (country fixed effects)",
    y = "Within R²"
  ) +
  theme_minimal()

ggsave("output/figures/squeeze_model_comparison.png", p2,
       width = 8, height = 6, dpi = 300)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Check output/tables/ and output/figures/ for results\n")

# Save models
save(m1_urban, m2_squeeze, m3_both, m4_components, m5_oecd_full, m6_growth,
     file = "output/squeeze_index_models.RData")
cat("Models saved to output/squeeze_index_models.RData\n")

# Prepare panel
panel_data <- data %>%
  group_by(iso3c) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  filter(year >= 2000, year <= 2020,
         !is.na(fertility_rate))

cat("Panel data:", n_distinct(panel_data$iso3c), "countries\n")
cat("Observations:", nrow(panel_data), "\n\n")

# Standardize key variables
panel_data <- panel_data %>%
  mutate(
    # Standardize for comparability
    #bundle_index_z = scale(bundle_cost_index_no_housing)[,1],
    squeeze_z = scale(resource_squeeze_no_housing)[,1],
    urban_z = scale(urbanization_rate)[,1],
    gdp_z = scale(log(gdp_pc_ppp))[,1],
    education_z = scale(female_secondary_enroll)[,1]
  )

# ============================================
# MODEL COMPARISONS
# ============================================

cat("=== RUNNING REGRESSIONS ===\n\n")

# Model 1: Baseline - Urbanization (our original proxy)
m1_urban <- feols(
  fertility_rate ~ urban_z + gdp_z + education_z | iso3c,
  data = panel_data,
  vcov = "hetero"
)

# Model 2: Cost Index instead of urbanization
m2_cost_index <- feols(
  fertility_rate ~ squeeze_z + gdp_z + education_z | iso3c,
  data = panel_data %>% filter(!is.na(squeeze_z)),
  vcov = "hetero"
)

# Model 3: Resource Squeeze (cost/income ratio)
m3_squeeze <- feols(
  fertility_rate ~ squeeze_z + gdp_z + education_z | iso3c,
  data = panel_data %>% filter(!is.na(squeeze_z)),
  vcov = "hetero"
)

# Model 4: Horse race - both urbanization and cost index
m4_both <- feols(
  fertility_rate ~ squeeze_z + urban_z + gdp_z + education_z | iso3c,
  data = panel_data %>% filter(!is.na(squeeze_z)),
  vcov = "hetero"
)

# Model 5: Component decomposition - which costs matter most?
# Only use components with sufficient data
# m5_components <- feols(
#   fertility_rate ~ food_index + health_index + 
#     education_index + utility_index + gdp_z + education_z | iso3c,
#   data = panel_data %>% filter(!is.na(food_index) | !is.na(health_index)),
#   vcov = "hetero"
# )

# Model 6: For OECD countries with full index (including housing)
m6_oecd_full <- feols(
  fertility_rate ~ resource_squeeze_full + gdp_z + education_z | iso3c,
  data = panel_data %>% filter(has_housing_data == TRUE),
  vcov = "hetero"
)

# ============================================
# RESULTS TABLE
# ============================================

cat("=== MODEL COMPARISON ===\n\n")

# Only include models that successfully ran
models <- list(
  "Urbanization\n(proxy)" = m1_urban,
  "Cost Index\n(no housing)" = m2_cost_index,
  "Resource Squeeze\n(cost/income)" = m3_squeeze,
  "Both" = m4_both
)

# Add full index if it has enough data
if (exists("m6_oecd_full") && nobs(m6_oecd_full) > 50) {
  models[["Full Index\n(OECD only)"]] = m6_oecd_full
}

# Add components if it worked
if (exists("m5_components") && nobs(m5_components) > 50) {
  models[["Components"]] = m5_components
}

modelsummary(
  models,
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs", "FE: iso3c"),
  output = "markdown"
)

# Save to file
modelsummary(
  models[1:4],  # Main models only
  stars = TRUE,
  output = "output/tables/cost_index_results.html",
  title = "Testing Minimum Viable Bundle Cost Index"
)

# ============================================
# KEY COMPARISONS
# ============================================

cat("\n\n=== KEY FINDINGS ===\n\n")

# Extract R-squared values
r2_urban <- r2(m1_urban)["wr2"]
r2_cost <- r2(m2_cost_index)["wr2"]
r2_squeeze <- r2(m3_squeeze)["wr2"]

cat("1. EXPLANATORY POWER (Within R²):\n")
cat(sprintf("   Urbanization proxy:   %.4f\n", r2_urban))
cat(sprintf("   Cost Index:           %.4f", r2_cost))
if (r2_cost > r2_urban) {
  cat(sprintf(" ✓ BETTER (%.4f improvement)\n", r2_cost - r2_urban))
} else {
  cat(sprintf(" (%.4f change)\n", r2_cost - r2_urban))
}
cat(sprintf("   Resource Squeeze:     %.4f\n", r2_squeeze))

# Coefficient comparison
cat("\n2. COEFFICIENT MAGNITUDES:\n")
cat(sprintf("   Urbanization:         %.4f\n", coef(m1_urban)["urban_z"]))
cat(sprintf("   Cost Index:           %.4f\n", coef(m2_cost_index)["squeeze_z"]))
cat(sprintf("   Resource Squeeze:     %.4f\n", coef(m3_squeeze)["squeeze_z"]))

# 3. Horse race
cat("\n3. HORSE RACE (both in same model):\n")
if (exists("m4_both")) {
  cat(sprintf("   Cost Index:           %.4f (p = %.4f)\n", 
              coef(m4_both)["squeeze_z"],
              summary(m4_both)$coeftable["squeeze_z", "Pr(>|t|)"]))
  cat(sprintf("   Urbanization:         %.4f (p = %.4f)\n",
              coef(m4_both)["urban_z"],
              summary(m4_both)$coeftable["urban_z", "Pr(>|t|)"]))
  cat(sprintf("   Within R²:            %.4f\n", r2(m4_both)["wr2"]))
  
  if (abs(coef(m4_both)["squeeze_z"]) > abs(coef(m4_both)["urban_z"])) {
    cat("   → Cost Index is the stronger predictor\n")
  } else {
    cat("   → Urbanization is the stronger predictor\n")
  }
}

# Component analysis
cat("\n4. WHICH COST COMPONENTS MATTER MOST?\n")
if (exists("m5_components")) {
  components <- c("food_index", "transport_index", "health_index", 
                  "education_index", "utility_index")
  for (comp in components) {
    if (comp %in% names(coef(m5_components))) {
      cat(sprintf("   %s: %.4f\n", 
                  gsub("_index", "", comp),
                  coef(m5_components)[comp]))
    }
  }
}

# ============================================
# VISUALIZATION
# ============================================

cat("\n=== Creating visualizations ===\n")

# Plot 1: Cost Index vs Fertility
plot_data <- panel_data %>%
  filter(!is.na(resource_squeeze_full), !is.na(fertility_rate))

if (nrow(plot_data) > 100) {
  p1 <- plot_data %>%
    filter(year == 2020) %>%
    ggplot(aes(x = resource_squeeze_full, y = fertility_rate)) +
    geom_point(aes(size = population, color = region), alpha = 0.6) +
    geom_smooth(method = "lm", se = TRUE, color = "darkred") +
    labs(
      title = "Minimum Viable Bundle Cost Index vs Fertility",
      subtitle = "2020 data - Direct measurement of resource pressure",
      x = "Bundle Cost Index (higher = more expensive)",
      y = "Total Fertility Rate",
      caption = "Index combines food, transport, healthcare, education, and utilities"
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  ggsave("output/figures/cost_index_vs_fertility.png", p1,
         width = 10, height = 7, dpi = 300)
}

# Plot 2: Compare predictive power
comparison_df <- data.frame(
  Model = c("Urbanization", "Cost Index", "Resource Squeeze"),
  Within_R2 = c(r2_urban, r2_cost, r2_squeeze),
  N = c(nobs(m1_urban), nobs(m2_cost_index), nobs(m3_squeeze))
)

p2 <- ggplot(comparison_df, aes(x = Model, y = Within_R2)) +
  geom_col(fill = "steelblue") +
  geom_text(aes(label = sprintf("%.3f", Within_R2)), vjust = -0.5) +
  ylim(0, max(comparison_df$Within_R2) * 1.1) +
  labs(
    title = "Model Comparison: Explanatory Power",
    subtitle = "Within-country R² (country fixed effects)",
    y = "Within R²"
  ) +
  theme_minimal()

ggsave("output/figures/model_comparison_r2.png", p2,
       width = 8, height = 6, dpi = 300)

cat("\n=== ANALYSIS COMPLETE ===\n")
cat("Check output/tables/ and output/figures/ for results\n")

# Save models
save(m1_urban, m2_cost_index, m3_squeeze, m4_both, m5_components, m6_oecd_full,
     file = "output/cost_index_models.RData")
cat("Models saved to output/cost_index_models.RData\n")
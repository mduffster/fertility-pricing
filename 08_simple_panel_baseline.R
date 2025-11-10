# Simple output for panel results
# Run this after the panel models are created

library(tidyverse)
library(fixest)

# The models should already exist from running step 6
# Let's extract and display results simply

cat("\n========================================")
cat("\n  PANEL REGRESSION RESULTS")
cat("\n  Testing Resource Pressure Theory")
cat("\n========================================\n")

# Model 1: Pooled OLS
cat("\n--- Model 1: Pooled OLS (No Fixed Effects) ---\n")
cat("Coefficients:\n")
print(coef(m1_pooled))
cat("\nR-squared:", round(r2(m1_pooled)["r2"], 3), "\n")
cat("N =", nobs(m1_pooled), "\n")

# Model 2: Country Fixed Effects
cat("\n--- Model 2: Country Fixed Effects ---\n")
cat("Coefficients:\n")
print(coef(m2_fe))
cat("\nWithin R-squared:", round(r2(m2_fe)["wr2"], 3), "\n")
cat("N =", nobs(m2_fe), "\n")

# Model 3: FE + Resource Pressure
cat("\n--- Model 3: FE + Resource Pressure Index ---\n")
cat("Coefficients:\n")
print(coef(m3_fe_pressure))
cat("\nWithin R-squared:", round(r2(m3_fe_pressure)["wr2"], 3), "\n")
cat("N =", nobs(m3_fe_pressure), "\n")

# Model 4: FE + Urbanization
cat("\n--- Model 4: FE + Urbanization ---\n")
cat("Coefficients:\n")
print(coef(m4_fe_urban))
cat("\nWithin R-squared:", round(r2(m4_fe_urban)["wr2"], 3), "\n")
cat("N =", nobs(m4_fe_urban), "\n")

# Model 5: FE + Year effects
cat("\n--- Model 5: FE + Year + Resource Pressure ---\n")
cat("Coefficients:\n")
print(coef(m5_fe_year))
cat("\nWithin R-squared:", round(r2(m5_fe_year)["wr2"], 3), "\n")
cat("N =", nobs(m5_fe_year), "\n")

# Model 6: FE + Year + Gap
cat("\n--- Model 6: FE + Year + Needs/Capacity Gap ---\n")
cat("Coefficients:\n")
print(coef(m6_fe_gap))
cat("\nWithin R-squared:", round(r2(m6_fe_gap)["wr2"], 3), "\n")
cat("N =", nobs(m6_fe_gap), "\n")

cat("\n========================================")
cat("\n  KEY INTERPRETATION")
cat("\n========================================\n")

cat("\nCountry Fixed Effects remove all time-invariant factors")
cat("\n(culture, geography, institutions, etc.)\n")

cat("\nWe're testing: WITHIN each country, as resource pressure")
cat("\nincreases over time, does fertility fall?\n")

# Compare Model 2 (baseline) vs Model 3 (with pressure)
if (exists("m2_fe") && exists("m3_fe_pressure")) {
  r2_baseline <- r2(m2_fe)["wr2"]
  r2_pressure <- r2(m3_fe_pressure)["wr2"]
  
  cat("\n*** Does Resource Pressure add explanatory power? ***")
  cat("\nWithin R² (baseline):", round(r2_baseline, 4))
  cat("\nWithin R² (+ pressure):", round(r2_pressure, 4))
  cat("\nImprovement:", round(r2_pressure - r2_baseline, 4), "\n")
  
  if (r2_pressure > r2_baseline) {
    cat("\n✓ Resource pressure DOES add explanatory power\n")
  } else {
    cat("\n✗ Resource pressure does NOT improve the model\n")
  }
}

# Get coefficient on urbanization from Model 4
if (exists("m4_fe_urban")) {
  urban_coef <- coef(m4_fe_urban)["urbanization_rate"]
  cat("\n*** Urbanization Effect (within countries) ***")
  cat("\nCoefficient:", round(urban_coef, 4))
  if (urban_coef < 0) {
    cat("\n✓ As countries urbanize, fertility FALLS (supports your theory)")
  } else {
    cat("\n✗ As countries urbanize, fertility RISES (contradicts your theory)")
  }
  cat("\n")
}

cat("\n========================================\n")
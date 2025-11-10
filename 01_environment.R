# Fertility Decline Analysis - Setup Script
# Author: [Your name]
# Date: 2025-09-30

# Install required packages (run once)
# install.packages(c(
#   "tidyverse",      # Data manipulation and viz
#   "WDI",            # World Bank data API
#   "OECD",           # OECD data API
#   "countrycode",    # Country name standardization
#   "fixest",         # Fast fixed effects models
#   "modelsummary",   # Nice regression tables
#   "ggplot2",        # Plotting
#   "scales",         # For formatting
#   "patchwork",      # Combining plots
#   "stargazer",      # Regression output
#   "plm",            # Panel data models
#   "haven"           # For reading various formats
# ))

# Load libraries
library(tidyverse)
library(WDI)
library(countrycode)
library(fixest)
library(modelsummary)
library(scales)
library(patchwork)

# Set theme for plots
theme_set(theme_minimal(base_size = 12))

# Function to clean country names
clean_countries <- function(df, country_col = "country") {
  df %>%
    mutate(
      iso3c = countrycode(!!sym(country_col), 
                          origin = "country.name", 
                          destination = "iso3c",
                          warn = FALSE),
      country_clean = countrycode(iso3c, 
                                   origin = "iso3c", 
                                   destination = "country.name",
                                   warn = FALSE)
    )
}

# Function to save outputs
save_output <- function(obj, name, type = "rds") {
  if (type == "rds") {
    saveRDS(obj, paste0("data/processed/", name, ".rds"))
  } else if (type == "csv") {
    write_csv(obj, paste0("data/processed/", name, ".csv"))
  }
  message(paste("Saved:", name))
}

print("Setup complete. Ready to download data.")
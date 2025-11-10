# Process OECD Housing Cost Burden Data
# Reads the HC1.2 Housing costs over income Excel file

library(tidyverse)
library(readxl)
library(countrycode)

# Function to read OECD housing data
read_oecd_housing <- function(filepath) {
  
  if (!file.exists(filepath)) {
    cat("OECD housing file not found at:", filepath, "\n")
    return(NULL)
  }
  
  cat("Reading OECD housing data...\n")
  
  # Read the sheet to get year column names
  year_header <- read_excel(
    filepath,
    sheet = "HC12_A1",
    skip = 4,
    n_max = 1
  )
  
  # Get year columns (skip first column which is country/category)
  year_cols <- names(year_header)[-1]
  year_cols <- year_cols[!is.na(year_cols) & year_cols != ""]
  
  cat("Found", length(year_cols), "year columns\n")
  
  # Read full data with no column names
  housing_raw <- read_excel(
    filepath,
    sheet = "HC12_A1",
    skip = 4,
    col_names = FALSE,
    .name_repair = "minimal"
  )
  
  # Keep only columns we need (first column + year columns)
  n_cols <- min(length(year_cols) + 1, ncol(housing_raw))
  housing_raw <- housing_raw[, 1:n_cols]
  
  # Name the columns
  col_names <- c("category", year_cols[1:(n_cols - 1)])
  names(housing_raw) <- col_names
  
  # Process: identify countries and extract rent data
  housing_clean <- housing_raw %>%
    mutate(
      # Country rows don't contain "Owner" or "Rent"
      is_country = !str_detect(category, "Owner|Rent|^\\s*$", negate = FALSE),
      country = if_else(is_country, category, NA_character_)
    ) %>%
    # Fill country down
    fill(country, .direction = "down") %>%
    # Filter for rent rows
    filter(str_detect(category, "Rent \\(private and subsidised\\)")) %>%
    select(-category, -is_country) %>%
    # Pivot to long
    pivot_longer(
      cols = -country,
      names_to = "year",
      values_to = "housing_burden_rent"
    ) %>%
    mutate(
      year = as.integer(year),
      housing_burden_rent = as.numeric(housing_burden_rent) * 100
    ) %>%
    filter(!is.na(housing_burden_rent))
  
  # Process mortgage data
  housing_mortgage <- housing_raw %>%
    mutate(
      is_country = !str_detect(category, "Owner|Rent|^\\s*$", negate = FALSE),
      country = if_else(is_country, category, NA_character_)
    ) %>%
    fill(country, .direction = "down") %>%
    filter(str_detect(category, "Owner with mortgage")) %>%
    select(-category, -is_country) %>%
    pivot_longer(
      cols = -country,
      names_to = "year",
      values_to = "housing_burden_mortgage"
    ) %>%
    mutate(
      year = as.integer(year),
      housing_burden_mortgage = as.numeric(housing_burden_mortgage) * 100
    ) %>%
    filter(!is.na(housing_burden_mortgage))
  
  # Merge both datasets
  housing_combined <- full_join(
    housing_clean,
    housing_mortgage,
    by = c("country", "year")
  ) %>%
    # Add ISO codes
    mutate(
      iso3c = countrycode(country, 
                          origin = "country.name",
                          destination = "iso3c",
                          warn = FALSE)
    ) %>%
    filter(!is.na(iso3c)) %>%
    # Create combined measure
    mutate(
      housing_burden_avg = case_when(
        !is.na(housing_burden_rent) & !is.na(housing_burden_mortgage) ~ 
          (housing_burden_rent + housing_burden_mortgage) / 2,
        !is.na(housing_burden_rent) ~ housing_burden_rent,
        !is.na(housing_burden_mortgage) ~ housing_burden_mortgage,
        TRUE ~ NA_real_
      )
    ) %>%
    select(iso3c, country, year, housing_burden_rent, 
           housing_burden_mortgage, housing_burden_avg) %>%
    arrange(country, year)
  
  return(housing_combined)
}

# Read the file
oecd_file <- "C:/Users/mdtel/OneDrive/Desktop/fertility pricing/data/HC1-2-Housing-costs-over-income.xlsx"

if (file.exists(oecd_file)) {
  housing_data <- read_oecd_housing(oecd_file)
  
  if (!is.null(housing_data)) {
    # Summary
    cat("\n=== OECD Housing Data Summary ===\n")
    cat("Countries:", n_distinct(housing_data$iso3c), "\n")
    cat("Years:", min(housing_data$year, na.rm = TRUE), "to", 
        max(housing_data$year, na.rm = TRUE), "\n")
    cat("Total observations:", nrow(housing_data), "\n")
    
    # Save
    dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
    saveRDS(housing_data, "data/processed/oecd_housing.rds")
    write_csv(housing_data, "data/processed/oecd_housing.csv")
    
    cat("\nOECD housing data saved successfully!\n")
    
    # Show sample
    cat("\nSample data:\n")
    print(head(housing_data, 10))
    
    # Summary by country
    cat("\nObservations per country:\n")
    housing_data %>%
      group_by(country) %>%
      summarise(
        n_years = n(),
        first_year = min(year),
        last_year = max(year),
        avg_burden = round(mean(housing_burden_avg, na.rm = TRUE), 1)
      ) %>%
      print(n = Inf)
  }
} else {
  cat("File not found:", oecd_file, "\n")
  cat("Please ensure the file path is correct\n")
}

cat("\n=== Done ===\n")
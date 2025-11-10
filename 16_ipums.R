# 17_ipums_usa_analysis.R
# Test employment structure theory using US census data

library(tidyverse)
library(ipumsr)
library(fixest)
library(modelsummary)

cat("=== IPUMS USA CENSUS ANALYSIS ===\n\n")

# Set API key
set_ipums_api_key("SET KEY HERE")

# USA samples covering period of fertility decline
samples <- c(
  "us2000a",
  "us2010a",
  "us2015a",
  "us2020a"
)

variables <- c(
  "METRO", "CITY", "FERTYR", "NCHILD", "AGE", "SEX",
  "EDUC", "CLASSWKR", "EMPSTAT", "OCC", "IND",
  "HHINCOME", "RENT", "VALUEH", "COSTELEC", "COSTGAS", "MORTGAGE"
)

cat("Submitting extract...\n")

extract <- define_extract_micro(
  collection = "usa",
  description = "Employment structure and fertility in US cities",
  samples = samples,
  variables = variables
)

submitted <- submit_extract(extract)
cat("Extract submitted. Waiting...\n")

extract_ready <- wait_for_extract(submitted)
download_extract(extract_ready)

cat("Download complete!\n\n")

# Find the downloaded files
ddi_file <- list.files(pattern = "\\.xml$", full.names = TRUE)[1]
data_file <- list.files(pattern = "\\.dat\\.gz$", full.names = TRUE)[1]

cat("Loading files:\n")
cat("  DDI:", ddi_file, "\n")
cat("  Data:", data_file, "\n\n")

# Load
ddi <- read_ipums_ddi(ddi_file)
data <- read_ipums_micro(ddi, data_file = data_file)

cat("Loaded:", nrow(data), "records\n\n")

# Clean
analysis_data <- data %>%
  filter(SEX == 2, AGE >= 25, AGE <= 45) %>%
  mutate(
    metro = (METRO %in% c(2, 3, 4)),
    big_city = (METRO == 2),
    
    self_employed = (CLASSWKR %in% c(1, 14)),
    wage_worker = (CLASSWKR %in% c(2, 23, 24, 25)),
    
    employment_type = case_when(
      self_employed ~ "Self-employed",
      wage_worker ~ "Wage worker",
      TRUE ~ "Not working"
    ),
    
    children_in_hh = NCHILD,
    
    education = case_when(
      EDUC <= 6 ~ "Less than HS",
      EDUC <= 9 ~ "High school",
      EDUC <= 10 ~ "Some college",
      EDUC >= 11 ~ "College+"
    ),
    
    housing_burden = case_when(
      RENT > 0 ~ (RENT * 12) / (HHINCOME + 1),
      MORTGAGE == 3 ~ (MORTGAGE * 12) / (HHINCOME + 1),
      TRUE ~ NA_real_
    ),
    
    high_housing_cost = (housing_burden > 0.3)
  ) %>%
  filter(!is.na(employment_type), !is.na(metro))

cat("Analysis sample:", nrow(analysis_data), "women\n\n")

# Models
m1 <- feols(
  children_in_hh ~ metro + factor(employment_type) +
    AGE + I(AGE^2) + factor(education) + log(HHINCOME + 1) | YEAR,
  data = analysis_data,
  weights = ~PERWT
)

m2 <- feols(
  children_in_hh ~ metro * factor(employment_type) + 
    AGE + I(AGE^2) + factor(education) + log(HHINCOME + 1) | YEAR,
  data = analysis_data,
  weights = ~PERWT
)

m3 <- feols(
  children_in_hh ~ high_housing_cost + factor(employment_type) +
    metro + AGE + I(AGE^2) + factor(education) + log(HHINCOME + 1) | YEAR,
  data = analysis_data %>% filter(!is.na(high_housing_cost)),
  weights = ~PERWT
)

# Results
modelsummary(
  list("Employment" = m1, "Metro×Emp" = m2, "Housing" = m3),
  stars = TRUE,
  output = "markdown"
)

cat("\n=== KEY FINDINGS ===\n\n")
emp_coef <- coef(m1)["factor(employment_type)Self-employed"]
cat(sprintf("Self-employed effect: %.3f\n", emp_coef))

save(m1, m2, m3, analysis_data, file = "output/ipums_usa_results.RData")

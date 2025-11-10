# Download World Bank Data
# This script downloads fertility, income, and other key indicators

library(tidyverse)
library(WDI)
library(countrycode)

# Define indicators we need
indicators <- c(
  "SP.DYN.TFRT.IN",    # Fertility rate (births per woman)
  "NY.GDP.PCAP.PP.KD", # GDP per capita, PPP (constant 2017 international $)
  "NY.ADJ.NNTY.PC.CD", # Adjusted net national income per capita
  "SE.SEC.ENRR.FE",    # School enrollment, secondary, female (% gross)
  "SE.SEC.ENRR",       # School enrollment, secondary (% gross) - alternative
  "SL.TLF.CACT.FE.ZS", # Female labor force participation rate
  "SP.POP.TOTL",       # Population, total
  "SP.URB.TOTL.IN.ZS", # Urban population (% of total)
  "SP.DYN.IMRT.IN",    # Infant mortality rate
  "SI.POV.GINI"        # GINI index (income inequality)
)

# Download data from 1990 to 2023
print("Downloading World Bank data...")
wb_data <- WDI(
  indicator = indicators,
  start = 1990,
  end = 2023,
  extra = TRUE  # This adds region, income level, etc.
)

# Clean and reshape
wb_clean <- wb_data %>%
  # Remove aggregates (regions, income groups)
  filter(region != "Aggregates") %>%
  # Rename variables to something more readable
  rename(
    fertility_rate = SP.DYN.TFRT.IN,
    gdp_pc_ppp = NY.GDP.PCAP.PP.KD,
    income_pc = NY.ADJ.NNTY.PC.CD,
    female_secondary_enroll = SE.SEC.ENRR.FE,
    secondary_enroll = SE.SEC.ENRR,
    female_lfp = SL.TLF.CACT.FE.ZS,
    population = SP.POP.TOTL,
    urban_pct = SP.URB.TOTL.IN.ZS,
    infant_mortality = SP.DYN.IMRT.IN,
    gini = SI.POV.GINI
  ) %>%
  # Keep relevant columns
  select(
    country, iso3c, iso2c, year, region, income,
    fertility_rate, gdp_pc_ppp, income_pc,
    female_secondary_enroll, secondary_enroll, female_lfp,
    population, urban_pct, infant_mortality, gini
  ) %>%
  # Remove rows where fertility is missing
  filter(!is.na(fertility_rate))

# Summary
print(paste("Downloaded data for", n_distinct(wb_clean$country), "countries"))
print(paste("Years:", min(wb_clean$year), "to", max(wb_clean$year)))
print(paste("Total observations:", nrow(wb_clean)))

# Check coverage for recent years
coverage_2022 <- wb_clean %>%
  filter(year == 2022) %>%
  summarise(
    n_countries = n(),
    pct_with_fertility = mean(!is.na(fertility_rate)) * 100,
    pct_with_gdp = mean(!is.na(gdp_pc_ppp)) * 100,
    pct_with_education = mean(!is.na(female_secondary_enroll)) * 100
  )

print("Coverage for 2022:")
print(coverage_2022)

# Save
saveRDS(wb_clean, "data/processed/worldbank_data.rds")
write_csv(wb_clean, "data/processed/worldbank_data.csv")

print("World Bank data saved!")
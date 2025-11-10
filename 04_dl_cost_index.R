# Download All Cost Components for Minimum Viable Bundle Index
# This will get: food, transport, healthcare, education, utilities

library(tidyverse)
library(WDI)
library(countrycode)

cat("=== DOWNLOADING COST DATA COMPONENTS ===\n\n")

# ============================================
# 1. FOOD PRICES
# ============================================

cat("1. Downloading food price data...\n")

# World Bank food price index and CPI data
food_indicators <- c(
  "FP.CPI.TOTL.ZG",     # Inflation, consumer prices (annual %)
  "AG.CON.FERT.PT.ZS"   # Fertilizer consumption (proxy for food input costs)
)

food_data <- WDI(
  indicator = food_indicators,
  start = 2000,
  end = 2023,
  extra = TRUE
) %>%
  filter(region != "Aggregates") %>%
  rename(
    inflation = FP.CPI.TOTL.ZG,
    fert_consumption = AG.CON.FERT.PT.ZS
  ) %>%
  select(iso3c, country, year, inflation, fert_consumption)

cat("   - Downloaded inflation/food proxy data:", nrow(food_data), "observations\n")

# ============================================
# 2. TRANSPORT/ENERGY COSTS  
# ============================================

cat("\n2. Downloading transport/energy costs...\n")

# World Bank doesn't have direct fuel price data anymore
# Use energy use and imports as proxies
transport_indicators <- c(
  "EG.USE.COMM.CL.ZS",    # Alternative and nuclear energy (% of total energy use)
  "IS.ROD.PAVE.ZS"        # Roads, paved (% of total roads)
)

transport_data <- WDI(
  indicator = transport_indicators,
  start = 2000,
  end = 2023,
  extra = TRUE
) %>%
  filter(region != "Aggregates") %>%
  rename(
    alt_energy_pct = EG.USE.COMM.CL.ZS,
    paved_roads_pct = IS.ROD.PAVE.ZS
  ) %>%
  mutate(
    # Proxy: More developed infrastructure = higher transport costs
    transport_proxy = paved_roads_pct
  ) %>%
  select(iso3c, country, year, alt_energy_pct, paved_roads_pct, transport_proxy)

cat("   - Downloaded transport proxy data:", nrow(transport_data), "observations\n")

# ============================================
# 3. HEALTHCARE COSTS
# ============================================

cat("\n3. Downloading healthcare costs...\n")

health_indicators <- c(
  "SH.XPD.CHEX.PC.CD",  # Current health expenditure per capita (current US$)
  "SH.XPD.OOPC.CH.ZS"   # Out-of-pocket expenditure (% of current health expenditure)
)

health_data <- WDI(
  indicator = health_indicators,
  start = 2000,
  end = 2023,
  extra = TRUE
) %>%
  filter(region != "Aggregates") %>%
  rename(
    health_expend_pc = SH.XPD.CHEX.PC.CD,
    health_oop_pct = SH.XPD.OOPC.CH.ZS
  ) %>%
  mutate(
    # Out-of-pocket health costs per capita
    health_oop_pc = health_expend_pc * (health_oop_pct / 100)
  ) %>%
  select(iso3c, country, year, health_expend_pc, health_oop_pct, health_oop_pc)

cat("   - Downloaded healthcare cost data:", nrow(health_data), "observations\n")

# ============================================
# 4. EDUCATION COSTS
# ============================================

cat("\n4. Downloading education costs...\n")

education_indicators <- c(
  "SE.XPD.TOTL.GB.ZS",  # Government expenditure on education (% of GDP)
  "SE.XPD.TOTL.GD.ZS"   # Government expenditure on education (% of government expenditure)
)

education_data <- WDI(
  indicator = education_indicators,
  start = 2000,
  end = 2023,
  extra = TRUE
) %>%
  filter(region != "Aggregates") %>%
  rename(
    edu_expend_gdp = SE.XPD.TOTL.GB.ZS,
    edu_expend_gov = SE.XPD.TOTL.GD.ZS
  ) %>%
  select(iso3c, country, year, edu_expend_gdp, edu_expend_gov)

cat("   - Downloaded education expenditure data:", nrow(education_data), "observations\n")

# ============================================
# 5. ELECTRICITY/UTILITIES
# ============================================

cat("\n5. Downloading electricity/utility data...\n")

utility_indicators <- c(
  "EG.ELC.ACCS.ZS",     # Access to electricity (% of population)
  "EG.USE.ELEC.KH.PC"   # Electric power consumption (kWh per capita)
)

utility_data <- WDI(
  indicator = utility_indicators,
  start = 2000,
  end = 2023,
  extra = TRUE
) %>%
  filter(region != "Aggregates") %>%
  rename(
    electricity_access = EG.ELC.ACCS.ZS,
    electricity_consumption_pc = EG.USE.ELEC.KH.PC
  ) %>%
  select(iso3c, country, year, electricity_access, electricity_consumption_pc)

cat("   - Downloaded electricity data:", nrow(utility_data), "observations\n")

# ============================================
# 6. GET INCOME DATA FOR DENOMINATOR
# ============================================

cat("\n6. Getting income data for burden calculation...\n")

income_indicators <- c(
  "NY.GDP.PCAP.PP.KD",  # GDP per capita, PPP
  "NY.ADJ.NNTY.PC.CD"   # Adjusted net national income per capita
)

income_data <- WDI(
  indicator = income_indicators,
  start = 2000,
  end = 2023,
  extra = TRUE
) %>%
  filter(region != "Aggregates") %>%
  rename(
    gdp_pc_ppp = NY.GDP.PCAP.PP.KD,
    income_pc = NY.ADJ.NNTY.PC.CD
  ) %>%
  select(iso3c, country, year, gdp_pc_ppp, income_pc, region)

cat("   - Downloaded income data:", nrow(income_data), "observations\n")

# ============================================
# MERGE ALL COST COMPONENTS
# ============================================

cat("\n7. Merging all cost components...\n")

all_costs <- income_data %>%
  left_join(food_data, by = c("iso3c", "country", "year")) %>%
  left_join(transport_data, by = c("iso3c", "country", "year")) %>%
  left_join(health_data, by = c("iso3c", "country", "year")) %>%
  left_join(education_data, by = c("iso3c", "country", "year")) %>%
  left_join(utility_data, by = c("iso3c", "country", "year"))

cat("   - Merged dataset:", nrow(all_costs), "observations\n")
cat("   - Countries:", n_distinct(all_costs$iso3c), "\n")
cat("   - Years:", min(all_costs$year), "to", max(all_costs$year), "\n")

# Check coverage
cat("\n=== DATA COVERAGE ===\n")
coverage <- all_costs %>%
  filter(year == 2020) %>%
  summarise(
    n_countries = n(),
    pct_transport = mean(!is.na(transport_proxy)) * 100,
    pct_health = mean(!is.na(health_oop_pc)) * 100,
    pct_education = mean(!is.na(edu_expend_gdp)) * 100,
    pct_electricity = mean(!is.na(electricity_consumption_pc)) * 100
  )

print(coverage)

# Save
dir.create("data/processed", showWarnings = FALSE, recursive = TRUE)
saveRDS(all_costs, "data/processed/cost_components.rds")
write_csv(all_costs, "data/processed/cost_components.csv")

cat("\n=== COST COMPONENTS SAVED ===\n")
cat("Next step: Run build_cost_index.R to create the bundle index\n")

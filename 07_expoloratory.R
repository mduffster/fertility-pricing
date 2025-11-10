# Exploratory Data Analysis
# Test basic relationships before formal modeling

library(tidyverse)
library(scales)
library(patchwork)

# Load data
data <- readRDS("data/processed/full_analysis_data.rds")

# Focus on most recent year with good coverage
recent_data <- data %>%
  filter(year == 2020) %>%
  filter(!is.na(fertility_rate), !is.na(gdp_pc_ppp))

# Plot 1: Classic relationship - Fertility vs GDP
p1 <- ggplot(recent_data, aes(x = gdp_pc_ppp, y = fertility_rate)) +
  geom_point(aes(size = population, color = region), alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  scale_x_log10(labels = dollar_format()) +
  labs(
    title = "The Classic Pattern: Fertility vs GDP",
    subtitle = "2020 data",
    x = "GDP per capita (PPP, log scale)",
    y = "Total Fertility Rate",
    size = "Population",
    color = "Region"
  ) +
  theme(legend.position = "bottom")

# Plot 2: Your theory - Fertility vs Resource Pressure
p2 <- ggplot(recent_data, aes(x = resource_pressure_index, y = fertility_rate)) +
  geom_point(aes(size = population, color = region), alpha = 0.6) +
  geom_smooth(method = "lm", se = TRUE, color = "black") +
  labs(
    title = "Your Theory: Fertility vs Resource Pressure",
    subtitle = "Resource Pressure = f(urbanization, income constraint)",
    x = "Resource Pressure Index (standardized)",
    y = "Total Fertility Rate",
    size = "Population",
    color = "Region"
  ) +
  theme(legend.position = "bottom")

# Plot 3: Fertility vs Urbanization
p3 <- ggplot(recent_data, aes(x = urbanization_rate, y = fertility_rate)) +
  geom_point(aes(size = population, color = region), alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(
    title = "Urbanization Effect",
    x = "Urban Population (%)",
    y = "Total Fertility Rate"
  ) +
  theme(legend.position = "none")

# Plot 4: Fertility vs Female Education (enrollment as proxy)
p4 <- ggplot(recent_data, aes(x = female_secondary_enroll, y = fertility_rate)) +
  geom_point(aes(size = population, color = region), alpha = 0.6) +
  geom_smooth(method = "loess", se = TRUE, color = "black") +
  labs(
    title = "Education Effect",
    x = "Female Secondary Enrollment (%)",
    y = "Total Fertility Rate"
  ) +
  theme(legend.position = "none")

# Combine plots
combined <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    title = "Fertility Decline: Testing Competing Explanations",
    theme = theme(plot.title = element_text(size = 16, face = "bold"))
  )

ggsave("output/figures/exploratory_four_panel.png", 
       combined, width = 14, height = 10, dpi = 300)

# Simple correlations
correlations <- recent_data %>%
  select(fertility_rate, gdp_pc_ppp, resource_pressure_index, 
         urbanization_rate, female_secondary_enroll) %>%
  cor(use = "pairwise.complete.obs") %>%
  as.data.frame() %>%
  rownames_to_column("variable") %>%
  select(variable, fertility_rate) %>%
  arrange(desc(abs(fertility_rate)))

print("Correlations with Fertility Rate (2020):")
print(correlations)

# Test your theory: Partial correlation
# Does resource pressure predict fertility AFTER controlling for GDP?

model_gdp_only <- lm(fertility_rate ~ log(gdp_pc_ppp), 
                     data = recent_data)

model_with_pressure <- lm(fertility_rate ~ log(gdp_pc_ppp) + resource_pressure_index, 
                          data = recent_data)

model_education <- lm(fertility_rate ~ log(gdp_pc_ppp) + female_secondary_enroll,
                      data = recent_data)

model_full <- lm(fertility_rate ~ log(gdp_pc_ppp) + resource_pressure_index + 
                   female_secondary_enroll,
                 data = recent_data)

# Compare models
library(modelsummary)

models <- list(
  "GDP Only" = model_gdp_only,
  "GDP + Resource Pressure" = model_with_pressure,
  "GDP + Education" = model_education,
  "Full Model" = model_full
)

modelsummary(
  models,
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs"),
  output = "output/tables/initial_regressions.html"
)

modelsummary(
  models,
  stars = TRUE,
  gof_map = c("r.squared", "adj.r.squared", "nobs")
)

print("\n=== Key Question ===")
print("Does Resource Pressure Index add explanatory power beyond GDP and Education?")
print(paste("R-squared (GDP only):", round(summary(model_gdp_only)$r.squared, 3)))
print(paste("R-squared (GDP + Pressure):", round(summary(model_with_pressure)$r.squared, 3)))
print(paste("R-squared (GDP + Education):", round(summary(model_education)$r.squared, 3)))
print(paste("R-squared (Full):", round(summary(model_full)$r.squared, 3)))

print("\nExploratory analysis complete! Check output/ folder for figures and tables.")
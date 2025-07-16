# scripts/analysis.R
# This script fits a simple linear model and saves the coefficients for later use.

library(readr)
library(dplyr)
library(broom)

# Load processed data
penguins_clean <- read_csv("data/processed/penguins_clean.csv")

# Simple Linear Model
model <- lm(bill_length_mm ~ bill_depth_mm + species + sex, data = penguins_clean)

# Save a text summary
capture.output(summary(model), file = "output/model_summary.txt")

# Save tidy model coefficients
coeffs <- tidy(model)
write_csv(coeffs, "output/model_coeffs.csv")

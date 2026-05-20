# scripts/clean-data.R
# This script loads the data, saves the raw version, cleans it slightly, and saves the processed version.

# Install packages, only if not installed yet

if (!all(c("palmerpenguins", "dplyr", "readr", "broom", "ggplot2") %in% rownames(installed.packages()))) {
  install.packages(setdiff(c("palmerpenguins", "dplyr", "readr", "broom", "ggplot2"), rownames(installed.packages())))
}


# Libraries ----
library(palmerpenguins)
library(dplyr)
library(readr)

# Save raw data ----
write_csv(penguins, "data/raw/penguins_raw.csv")

# Remove rows with missing bill measurements or sex
penguins_clean <- penguins %>%
  filter(!is.na(bill_length_mm), !is.na(bill_depth_mm), !is.na(sex)) %>%
  select(species, island, bill_length_mm, bill_depth_mm, sex, year)

# Save processed data ----
write_csv(penguins_clean, "data/processed/penguins_clean.csv")

# Install the package only if missing
if (!requireNamespace("palmerpenguins", quietly = TRUE)) install.packages("palmerpenguins")

# Load packages ----
library(palmerpenguins)
library(dplyr)
library(ggplot2) #load ggplot!
library(lubridate) #load lubridate!

# Check package versions for reproducible workflow ----
packageVersion("palmerpenguins") #‘0.1.1’
packageVersion("dplyr") #‘1.1.3’
packageVersion("ggplot2") #‘3.5.0’
packageVersion("lubridate") #‘1.9.3’

# Load dataset and remove NAs  ----
data("penguins")
penguins <- na.omit(penguins) # option: overwrite original `penguins` object, to keep the environment clean

# Categorize body mass ----
penguins <- penguins %>%
  mutate(body_mass_category = ifelse(body_mass_g > 4000, "Large", "Small"))

# Generate random sample ----
set.seed(123) #Set seed for reproducibility!
penguins <- penguins %>%
  mutate(randSample = sample(1:100, n(), replace = TRUE))

# Calculate mean bill length and flipper length by species 
species_summary <- penguins %>%
  group_by(species) %>% #Use group_by and summarize!
  summarize(mean_bill_length = mean(bill_length_mm, na.rm = TRUE),
            mean_flipper_length = mean(flipper_length_mm, na.rm = TRUE),
            mean_body_mass = mean(body_mass_g, na.rm = TRUE))

print(species_summary) #visualize the result!

# Calculate Mean Mass ----
# option1: use loop
species_unique <- unique(penguins$species)
mean_mass <- setNames(numeric(length(species_unique)), species_unique)

for (spec in species_unique) {
  mean_mass[spec] <- mean(penguins$body_mass_g[penguins$species == spec], na.rm = TRUE)
}

print(mean_mass)

# option2: groupby and summarize
species_mass <- penguins %>%
  group_by(species) %>%
  summarize(mean_body_mass = mean(body_mass_g, na.rm = TRUE))

print(species_mass)

# Categorize flipper length ----
penguins <- penguins %>% #no loop is needed! 
  mutate(flipper_category = case_when(flipper_length_mm > 200 ~ "Long", #keep naming consistent (e.g. always snake_case)
                                      TRUE ~ "Short"))

# Merge IDs into the dataset ----

# Select variables based on column name 
#options: overwrite or create a new df
penguins_raw_edited <- penguins_raw %>% select(
  "Culmen Length (mm)", "Culmen Depth (mm)", "Flipper Length (mm)",
  "Body Mass (g)", "Date Egg", "Clutch Completion")

# Rename variables to match the penguin dataset
penguins_raw_edited <- penguins_raw_edited %>% rename(
  "bill_length_mm" = "Culmen Length (mm)",
  "bill_depth_mm" = "Culmen Depth (mm)",
  "flipper_length_mm" = "Flipper Length (mm)",
  "body_mass_g" = "Body Mass (g)",
  "Date_egg" = "Date Egg",
  "Clutch_completion" = "Clutch Completion")

# Merge into the penguin dataset:
# Separated over lines & specify merge for x variable 
penguins <- merge(x = penguins, y = penguins_raw_edited, 
                  by = c("bill_length_mm","bill_depth_mm",
                         "flipper_length_mm","body_mass_g"), 
                  all.x = T)

# Generate fitness ----
constant1 <- 0.1 # constant for the variable Egg_date, also does not overwrite c
constant2 <- 1 # constant2 for the variable flipper_category

# Change egg date variable into julian egg day 
penguins$Lay_date <- lubridate::yday(as.Date(penguins$"Date_egg"))

# Generate fitness, does not overwrite 
penguins$W = ifelse(penguins$Clutch_completion  == "Yes",
                    1.5 + scale(penguins$Lay_date, center = TRUE, scale = FALSE) * constant1 +
                      as.numeric(as.factor(penguins$flipper_category))* constant2 +
                      + rnorm(nrow(penguins), mean = 0, sd = 0.25),
                    0) 

# Filter for only breeding individuals
breeders <- penguins %>% filter(Clutch_completion == "Yes")

# Linear model
lm_mod1 <- lm(W ~ flipper_category + Lay_date, 
              data = breeders) 

summary(lm_mod1)

# Filter Adelie penguins 
adeliePenguins <- penguins %>%
  filter(species == "Adelie")

# Plots ----

# Generate boxplot for body mass by species
ggplot(penguins, aes(x = species, y = body_mass_g, color = species)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Body Mass Distribution by Species", y = "Body Mass (g)")

# Generate scatter plot for flipper length vs. bill length
ggplot(penguins, aes(x = flipper_length_mm, y = bill_length_mm, color = species)) +
  geom_point() +
  theme_bw() + #Consistent theme!
  labs(title = "Flipper Length vs. Bill Length") #add labels

# Generate scatter and line plot for fitness vs. lay date
ggplot(breeders, aes(x = Lay_date, y = W, 
                     group = flipper_category, colour = flipper_category)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_bw() +
  labs(y = "Fitness", x = "Lay date")

# Generate boxplot for Adelie penguins with correct color mapping
ggplot(adeliePenguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Body Mass Distribution of Adelie Penguins")

# Save data as CSV ----
# When working with Rprojects no local path needs to be specified (reproducible!)

# Save the Adelie pinguin dataset as CSV.
write.csv(adeliePenguins, "Adelie_pinguin_data.csv", row.names = FALSE)

# If you want the date in the saved dataset
write.csv(adeliePenguins, paste(getwd(),"//","Adelie_pinguin_data_",
                                format(Sys.Date(), "%d.%m.%Y"), ".csv", sep = ""), 
          row.names = FALSE)

# End of script ----


# Install the package only if missing
if (!requireNamespace("palmerpenguins", quietly = TRUE)) install.packages("palmerpenguins")

# Load packages ----
library(palmerpenguins)
library(dplyr)
library(ggplot2) #load ggplot!

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

# Filter Adelie penguins 
adeliePenguins <- penguins %>%
  filter(species == "Adelie")

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

# Generate boxplot for Adelie penguins with correct color mapping
ggplot(adeliePenguins, aes(x = species, y = body_mass_g, fill = species)) +
  geom_boxplot() +
  theme_bw() +
  labs(title = "Body Mass Distribution of Adelie Penguins")

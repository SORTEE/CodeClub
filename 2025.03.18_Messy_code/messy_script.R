if (!requireNamespace("palmerpenguins", quietly = TRUE)) install.packages("palmerpenguins")

library(palmerpenguins)
library(dplyr)

data("penguins")

penguinsData=na.omit(penguins) #remove NAs

penguinsData$newCol = NA # add new column
for (i in 1:nrow(penguinsData)){
  if(penguinsData$body_mass_g[i] > 4000){
    penguinsData$newCol[i] = "Large"
  } else {
    penguinsData$newCol[i] = "small"  } } 

penguinsData$randSample <- sample(1:100, nrow(penguinsData), replace = TRUE) # No set.seed()


species_unique <- unique(penguinsData$species)

avgbill = c()
for (spec in unique(penguinsData$species)){
  avgbill = c(avgbill, mean(penguinsData$bill_length_mm[penguinsData$species == spec], na.rm=TRUE)) }
names(avgbill) = unique(penguinsData$species)

# Another redundant operation: Manually calculating average flipper length instead of using summarize
flipper_avg = c()
for (spec in unique(penguinsData$species)){
  flipper_avg = c(flipper_avg, mean(penguinsData$flipper_length_mm[penguinsData$species == spec], na.rm=TRUE)) }
names(flipper_avg) = unique(penguinsData$species)

mean_mass_Adelie <- mean(penguinsData$body_mass_g[penguinsData$species == "Adelie"], na.rm = TRUE)
mean_mass_Chinstrap <- mean(penguinsData$body_mass_g[penguinsData$species == "Chinstrap"], na.rm = TRUE)
mean_mass_Gentoo <- mean(penguinsData$body_mass_g[penguinsData$species == "Gentoo"], na.rm = TRUE)

ggplot(penguinsData, aes(x=species, y=body_mass_g, col=species)) + geom_boxplot() + theme_bw() #plot
ggplot(penguinsData, aes(x=flipper_length_mm, y=bill_length_mm)) +  geom_point(aes(color=species))

penguinsData$species_factor = as.factor(penguinsData$species) # unnecessary, species is already a factor

# Creating a categorical column for flipper size inefficiently
penguinsData$flipperCategory = NA
for (i in 1:nrow(penguinsData)){
  if (penguinsData$flipper_length_mm[i] > 200){
    penguinsData$flipperCategory[i] = "Long"
  } else {
    penguinsData$flipperCategory[i] = "Short"
  }
}

# Manually filtering Adelie penguins instead of using dplyr filter
adeliePenguins = penguinsData[penguinsData$species=="Adelie",]

# Plot Adelie penguin body mass distribution
ggplot(adeliePenguins, aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot() + 
  theme_minimal() +
  scale_fill_manual(values=c("blue", "red", "green"))

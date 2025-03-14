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

penguinsData$randSample <- sample(1:100, nrow(penguinsData), replace = T) # No set.seed()


species_unique <- unique(penguinsData$species)


avgbill = c()
for (spec in unique(penguinsData$species)){
  avgbill = c(avgbill, mean(penguinsData$bill_length_mm[penguinsData$species == spec], na.rm=TRUE)) }
names(avgbill) = unique(penguinsData$species)

flipper_avg = c()
for (spec in unique(penguinsData$species)){
  flipper_avg = c(flipper_avg, mean(penguinsData$flipper_length_mm[penguinsData$species == spec], na.rm=TRUE)) }
names(flipper_avg) = unique(penguinsData$species)

mean_mass_Adelie <- mean(penguinsData$body_mass_g[penguinsData$species == "Adelie"], na.rm = TRUE)
mean_mass_Chinstrap <- mean(penguinsData$body_mass_g[penguinsData$species == "Chinstrap"], na.rm = TRUE)
mean_mass_Gentoo <- mean(penguinsData$body_mass_g[penguinsData$species == "Gentoo"], na.rm = TRUE)

ggplot(penguinsData, aes(x=species, y=body_mass_g, col=species)) + geom_boxplot() + theme_bw() #plot
ggplot(penguinsData, aes(x=flipper_length_mm, y=bill_length_mm)) +  geom_point(aes(color=species))


penguinsData$species_factor = as.factor(penguinsData$species) 

penguinsData$flipperCategory = NA
for (i in 1:nrow(penguinsData)){
  if (penguinsData$flipper_length_mm[i] > 200){
    penguinsData$flipperCategory[i] = "Long"
  } else {
    penguinsData$flipperCategory[i] = "Short"
  }
}

attach(penguins_raw)
penguins_raw <- penguins_raw[,7:13]
names(penguins_raw)[names(penguins_raw) == "Culmen Length (mm)"] <- "bill_length_mm";names(penguins_raw)[names(penguins_raw) == "Culmen Depth (mm)"] <- "bill_depth_mm";names(penguins_raw)[names(penguins_raw) == "Body Mass (g)"] <- "body_mass_g";names(penguins_raw)[names(penguins_raw) == "Flipper Length (mm)"] <- "flipper_length_mm"
detach(penguins_raw)

test <- merge(penguinsData, penguins_raw, by = c("bill_length_mm","bill_depth_mm","flipper_length_mm","body_mass_g"), all = T)
penguinsData = na.omit(test) 

c <- 0.1 # constant
c2 <- 1 # constant2

#penguinsData$Date.Egg <- lubridate::yday(as.Date(penguinsData$"Date Egg"))
#penguinsData$Date.Egg <- lubridate::as.Date(penguinsData$"Date Egg")


penguinsData$W = ifelse(penguinsData$"Clutch Completion"  == "Yes",
                        1.5 + scale(penguinsData$Date.Egg, center = T, scale = F) * c + as.numeric(as.factor(penguinsData$flipperCategory))*c2, + rnorm(nrow(penguinsData), 0, 0.25), 
                        0)

mod1<-lm(W~flipperCategory+`Date Egg`+`Clutch Completion`, data = penguinsData) 
summary(mod1)

#plot the relationship
ggplot(penguinsData, aes(x=`Date Egg`, y=W)) +
  geom_smooth(method = "lm") +
  geom_point() +
  theme_classic()

adeliePenguins <- penguinsData[penguinsData$species=="Adelie",]

ggplot(adeliePenguins, aes(x=species, y=body_mass_g, fill=species)) + 
  geom_boxplot() + 
  theme_minimal() +
  scale_fill_manual(values=c("blue", "red", "green"))

write.csv(adeliePenguins, "SORTEE_Code_Club//Messy_code//PA_data.csv", row.names = F)
install.packages("palmerpenguins")
install.packages("tidyverse")
library(palmerpenguins)
library(tidyverse)
View(penguins)
dim(penguins)
str(penguins)
summary(penguins)
colSums(is.na(penguins))
feature_names <- names(penguins)
print(feature_names)
penguins%>%
  names()
penguins
glimpse(penguins)
# add data points
ggplot(data = penguins) +
  aes(x = bill_length_mm, y = bill_depth_mm) +
  geom_point()
# Create a scatterplot with different colors and symbol types using penguins data
ggplot(data = penguins) +
  aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species) +
  geom_point()
# Create a density plot with different fill colors for each species using penguins data
ggplot(data = penguins) +
  aes(x = bill_length_mm, fill = species) +
  geom_density(alpha = 0.3)
# Create a facet grid plot with palmerspenguins data
ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm, color = species)) +
  geom_point() +
  facet_grid(. ~ species) +
  theme_bw() +
  ggtitle("Penguins bill length and bill depth (facet_grid)")
penguins %>%
  count(species)
penguins %>%
  count(species, sort=TRUE)
count(penguins, species)
count(penguins, species, island)
penguins %>%
  count(species,island, sort=TRUE)
any(is.na(penguins))
is.na(penguins)
newdata <- na.omit(penguins)

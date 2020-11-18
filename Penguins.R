library(tidyverse)

# load package to access the data
library(palmerpenguins)

# clean variable names
penguin <- penguins_raw %>%
  janitor::clean_names()

# do a summary to look at the four variables used
penguin %>%
  select(body_mass_g,
         ends_with("_mm")) %>%
  summary()

# filter out the rows with missing values
penguin <- penguin %>%
  filter(!is.na(body_mass_g))

# split species into common_name and scientific_name
penguin <- penguin %>% 
  extract(species, 
          c("common_name", "scientific_name"),
          "([a-zA-Z]+\\s[a-zA-Z]+)\\s\\(([a-zA-Z]+\\s[a-zA-Z]+)\\)")

# load package GGally
library(GGally)

# select variables of interest and pipe in to ggpairs()
penguin %>%
  select(common_name, 
         sex, 
         island,
         body_mass_g,
         ends_with("_mm")) %>%
  ggpairs(aes(color = common_name))

# select the four variables and do PCA
pca <- penguin %>% 
  select(body_mass_g,
         ends_with("_mm")) %>%
  prcomp(scale. = TRUE)

summary(pca)
pca$rotation

# Extract the scores into a dataframe with the species name
pca_labelled <- data.frame(pca$x, common_name = penguin$common_name)
# scatterplot
pca_labelled %>% 
  ggplot(aes(x = PC1, y = PC2, color = common_name)) +
  geom_point() +
  scale_color_manual(values = c("blue", "yellow", "red"))

# Notes
# PC2 is completely uncorrelated with PC1
# PC1 = ax + by  <- is linear as there are no powers, a and b are 'loadings'

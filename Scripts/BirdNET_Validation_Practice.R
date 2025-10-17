################################################################################
# Title: Valadating BirdNET classifications
# Author: Will Harrod
# Date Created: 2025-10-15
################################################################################

# 1) Preparation ###############################################################

# 1.1) Add packages and data ---------------------------------------------------

# Clear environments
rm(list = ls())

# Add packages
library(tidyverse)
library(nimble)

# Add data 
birds <- read.csv("Data\\bear_river_birdnet_detections_june_2023.csv") %>% 
  select(common_name, confidence, ARU.ID, Date, start.min, start.sec) %>% 
  rename(Species = "common_name") 

# View data 
slice_head(birds, n = 30)
glimpse(birds)

# How many observations of each species 
birds %>% 
  count(Species) %>% 
  arrange(-n)

# View a specific species 
birds %>% 
  filter(Species == "Juniper Titmouse") %>% 
  arrange(confidence)

# Select the top species 
top_birds_list <- birds %>% 
  count(Species) %>% 
  arrange(-n) %>% 
  slice_head(n = 9) %>% 
  pull(Species)

# View top species 
top_birds_list

# Extract the top species detections from the list 
common_birds <- birds %>% 
  filter(Species %in% top_birds_list)

# View how many detections of those birds 
slice_head(common_birds, n = 30)
glimpse(common_birds)

# Plot by confidence 
common_birds %>% 
  ggplot() + 
  geom_boxplot(aes(x = Species, y = confidence, color = Species), size = 1) +
  scale_color_discrete() + 
  scale_y_continuous(limits = c(0.3, 1)) +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) 

# Plot number of detections by species
common_birds %>% 
  count(Species) %>% 
  ggplot() +
  geom_col(aes(x = Species, y = n, color = Species, fill = Species)) +
  scale_color_discrete() + 
  scale_fill_discrete() +
  theme_classic() +
  theme(axis.text.x = element_blank(),
        axis.title.x = element_blank()) 

# Define how many birds I want to validate
n_valid <- 100

# Select random rows to validate 
birds_to_validate <- common_birds %>% 
  group_by(Species) %>% 
  slice_sample(n = n_valid, replace = FALSE) %>% 
  # Arrange in a useful order
  arrange(Species, ARU.ID, Date, start.min, start.sec) %>% 
  # Add a column for whether or not each row is a true positive
  mutate(True.Positive = NA)

# View
print(birds_to_validate, n = Inf)

# Save as a csv 
write.csv(birds_to_validate, "Data\\birds_to_validate_bear_river_june_2023.csv")



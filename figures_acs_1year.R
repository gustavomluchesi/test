#######################################################################
# Script: figures_acs_1year.R
# Author: Gustavo Luchesi
# Last Updated: 8/22/2024
# Description: Create figures of birth rates for different cohorts of women

# Input: ACS PUMS 2000 - 2022 cleaned csv

# Output: Fertility by cohort figures 
#######################################################################

# Installing required packages
packages <- c("tidyverse")

to_install <- packages[!(packages %in% installed.packages()[,"Package"])]

if(length(to_install) > 0) install.packages(to_install)

lapply(packages, require, character.only = TRUE)

# Clean workspace
rm(list = ls())


pums_2008 <- read.csv("raw_data/pus2008.csv")


summary <- pums_2008 %>% 
  
  filter(!is.na(FER)) %>% 
  group_by(AGEP, FER) %>%  
  summarise(n_weighted = sum(PWGTP)) %>% 
  
  group_by(AGEP) %>% 
  mutate(total_weighted = sum(n_weighted)) %>% 
  ungroup() %>% 
  
  mutate(percent_pregnant = n_weighted / total_weighted) %>%
  filter(FER == "1") %>% 
  select(-c(FER, total_weighted, n_weighted))


summary %>% ggplot() +
  geom_line(aes(x = AGEP, y = percent_pregnant)) +
  labs(title = "Did you give birth to a child in the last 12 months?",
       x = "Age of Woman") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                     limits = c(0, 0.13),
                     n.breaks = 6) +
  theme_minimal() +
  theme(axis.title.y = element_blank())

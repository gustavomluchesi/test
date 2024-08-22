#######################################################################
# Script: figures_acs_1year.R
# Author: Gustavo Luchesi
# Last Updated: 8/21/2024
# Description: Create figures of birth rates for different cohorts of women

# Input: ACS PUMS 2000 - 2022 cleaned csv

# Output: Figures 
#######################################################################

install.packages("tidyverse")
install.packages("tidycensus")

library(tidyverse)
library(tidycensus)

vars <- c("SERIALNO", "SPORDER", "SEX", "AGEP", "FER")

us_pums <- get_pums(variables = vars,
                    state = "all",
                    survey = "acs1",
                    year = 2022)


women_us <- us_pums %>% 
  filter(FER != "b")

summary_2022 <- women_us %>% 
  
  group_by(AGEP, FER) %>%  
  summarise(n_weighted = sum(PWGTP)) %>% 
  
  group_by(AGEP) %>% 
  mutate(total_weighted = sum(n_weighted)) %>% 
  ungroup() %>% 
  
  mutate(percent_pregnant = n_weighted / total_weighted) %>%
  filter(FER == "1") %>% 
  select(-c(FER, total_weighted, n_weighted))


summary_2022 %>% ggplot() +
  geom_line(aes(x = AGEP, y = percent_pregnant)) +
  labs(title = "Did you give birth to a child in the last 12 months?",
       x = "Age of Woman") +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1),
                     limits = c(0, 0.125),
                     n.breaks = 6) +
  theme_minimal() +
  theme(axis.title.y = element_blank())

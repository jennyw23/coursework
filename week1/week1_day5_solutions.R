
library(tidyverse)
library(ggplot2)

table1
table2
table3
table4a
table4b

########################### Exercises 12.2.1 ##################################
# Compute the rate for table2, and table4a + table4b. You will need to perform four operations:
  # a Extract the number of TB cases per country per year.
  # b Extract the matching population per country per year.
  # c Divide cases by population, and multiply by 10000.
  # d Store back in the appropriate place.
# Which representation is easiest to work with? Which is hardest? Why?

# Table 2 
table2 %>%
  pivot_wider(names_from=type, values_from=count) %>% 
  mutate(rate=cases/population*10000)


# Combination of Tables 4a and 4b
table4a_long <- table4a %>%
  pivot_longer(names_to="year", values_to="cases", 2:3)
  
table4b_long <- table4b %>% 
  pivot_longer(names_to="year", values_to="population", 2:3)

table4a_long %>% 
  left_join(table4b_long, by=c("country", "year")) %>% 
  mutate(rate=cases/population*10000)

##############################################################################


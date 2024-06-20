
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

################ Section 12.3.3, exercise 1 ##################################

# Why are pivot_longer() and pivot_wider() not perfectly symmetrical?
#     Carefully consider the following example:

stocks <- tibble(
  year   = c(2015, 2015, 2016, 2016),
  half  = c(   1,    2,     1,    2),
  return = c(1.88, 0.59, 0.92, 0.17)
)
stocks %>% 
  pivot_wider(names_from = year, values_from = return) %>% 
  pivot_longer(`2015`:`2016`, names_to = "year", values_to = "return")
# (Hint: look at the variable types and think about column names.)
# pivot_longer() has a names_ptypes argument, e.g.  names_ptypes = list(year = double()). What does it do?

# The functions pivot_longer() and pivot_wider() are not perfectly symmetrical 
# because column type information is lost when a data frame is converted from wide
# to long. The function pivot_longer() stacks multiple columns which may have had 
#multiple data types into a single column with a single data type. This transformation
#throws away the individual data types of the original columns. The function
#pivot_wider() creates column names from values in column. These column names will
#always be treated as character values by pivot_longer() so if the original variable
#used to create the column names did not have a character data type, then the
#round-trip will not reproduce the same dataset.

################ Section 12.3.3, exercise 3 ##################################


people <- tribble(
  ~name,             ~names,  ~values,
  #-----------------|--------|------
  "Phillip Woods",   "age",       45,
  "Phillip Woods",   "height",   186,
  "Phillip Woods",   "age",       50,
  "Jessica Cordero", "age",       37,
  "Jessica Cordero", "height",   156
)

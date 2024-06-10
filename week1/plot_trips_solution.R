########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)
library(lubridate)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
setwd('//wsl.localhost/Ubuntu/home/v-wangjen/jenny_projects/coursework/week1/')
load('trips.RData')


########################################
# plot trip data
########################################



# plot the distribution of trip times across all rides
trips %>%
  ggplot(aes(x = tripduration )) + 
  geom_histogram(bins = 30)

trips %>%
  filter(tripduration <= 60 * 60) %>%
  ggplot(aes(x = tripduration )) + 
  geom_histogram(bins = 30)

trips %>%
  filter(tripduration <= 60 * 60) %>%
  ggplot(aes(x = tripduration )) + 
  geom_density(bins = 30)

# plot the distribution of trip times by rider type
trips %>%
  filter(tripduration <= 60 * 60) %>%
  ggplot(aes(x = tripduration / 60, color=usertype, fill=usertype)) + 
  geom_histogram(bins = 30) + 
  facet_wrap(~ usertype, scale = "free_y")

trips %>%
  filter(tripduration <= 60 * 60) %>%
  ggplot(aes(x = tripduration / 60, color=usertype, fill=usertype)) + 
  geom_density(bins = 30) + 
  facet_wrap(~ usertype, scale = "free_y")

trips %>%
  filter(tripduration <= 60 * 60) %>%
  ggplot(aes(x = tripduration, color=usertype, fill=usertype)) + 
  geom_density() + 
  facet_wrap(~ usertype, scale = "free_y")

# plot the total number of trips over each day
trips %>%
  group_by(ymd) %>%
  summarize(num_trips = n()) %>%
  ggplot(aes(x = ymd, y = num_trips)) +
  geom_point()

# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)
trips %>%
  mutate(age = 2019 - birth_year) %>%
  group_by(age, gender) %>%
  summarize(num_trips = n()) %>%
  ggplot(aes(x = age, y = num_trips, color = gender)) +
  geom_point() + 
  ylim(c(0, 250000))

# plot the ratio of male to female trips (on the y axis) by age (on the x axis)
# hint: use the spread() function to reshape things to make it easier to compute this ratio
trips %>%
  mutate(age = 2014 - birth_year) %>%
  group_by(age, gender) %>%
  summarize(num_trips = n()) %>%
  spread(gender, num_trips) %>%
  mutate(ratio = Male / Female) %>%
  ggplot(aes(x = age, y = ratio)) + 
 # xlim(c(18, 65)) +
  #ylim(c(0, 6)) +
  geom_point()

########################################
# plot weather data
########################################
# plot the minimum temperature (on the y axis) over each day (on the x axis)
weather %>%
  ggplot(aes(x = ymd, y = tmin)) + 
  geom_point()

# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
# hint: try using the gather() function for this to reshape things before plotting
weather %>%
  gather(max_min, temp, tmin, tmax) %>%
  ggplot(aes(x = ymd, y = temp, color = max_min)) + 
  geom_point()

########################################
# plot trip and weather data
########################################

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by="ymd")


# plot the number of trips as a function of the minimum temperature, where each point represents a day
# you'll need to summarize the trips and join to the weather data to do this
trips_with_weather %>%
  group_by(ymd) %>%
  summarize(num_trips = n(), min_temp = mean(tmin)) %>%
  ggplot(aes(x= min_temp, y = num_trips)) + 
  geom_point()

# repeat this, splitting results by whether there was substantial precipitation or not
# you'll need to decide what constitutes "substantial precipitation" and create a new T/F column to indicate this
trips_with_weather %>%
  group_by(ymd) %>%
  summarize(num_trips = n(), min_temp = mean(tmin), prcp = mean(prcp)) %>%
  mutate(rainy_day = prcp > .2) %>%
  ggplot(aes(x= min_temp, y = num_trips, color = rainy_day)) + 
  geom_point()

# add a smoothed fit on top of the previous plot, using geom_smooth
trips_with_weather %>%
  group_by(ymd) %>%
  summarize(num_trips = n(), min_temp = mean(tmin), prcp = mean(prcp)) %>%
  mutate(rainy_day = prcp > .2) %>%
  ggplot(aes(x= min_temp, y = num_trips, color = rainy_day)) + 
  geom_point() + 
  geom_smooth(method = "lm")

# compute the average number of trips and standard deviation in number of trips by hour of the day
# hint: use the hour() function from the lubridate package
trips %>%
  mutate(hr = hour(starttime)) %>%
  group_by(hr, ymd) %>%
  summarize(n_trips = n()) %>%
  group_by(hr) %>%
  summarize(mean = mean(n_trips), sd = sd(n_trips)) %>%
  ggplot(aes(x = hr, y = mean)) + 
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = .2) + 
  geom_line()

# plot the above

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
# hint: use the wday() function from the lubridate package
trips %>%
  mutate(hr = hour(starttime), dow = wday(starttime)) %>%
  group_by(hr, ymd, dow) %>%
  summarize(n_trips = n()) %>%
  group_by(hr, dow) %>%
  summarize(mean = mean(n_trips), sd = sd(n_trips)) %>%
  ggplot(aes(x = hr, y = mean)) + 
  geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd), alpha = .2) + 
  geom_line() + 
  facet_wrap(~dow)

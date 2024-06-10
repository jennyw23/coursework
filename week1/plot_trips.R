########################################
# load libraries
########################################

# load some packages that we'll need
library(tidyverse)
library(scales)

# be picky about white backgrounds on our plots
theme_set(theme_bw())

# load RData file output by load_trips.R
load('trips.RData')


########################################
# plot trip data
########################################

# plot the distribution of trip times across all rides (compare a histogram vs. a density plot)

ggplot(trips, aes(x = trip_time)) +
    geom_histogram() +
    ggtitle("Distribution of Trip Times")

ggplot(trips, aes(x = trip_time)) +
    geom_density() +
    ggtitle("Density Plot of Trip Times")

# plot the distribution of trip times by rider type indicated using color and fill (compare a histogram vs. a density plot)

ggplot(trips, aes(x = trip_time, fill = rider_type, color = rider_type)) +
    geom_histogram() +
    ggtitle("Distribution of Trip Times by Rider Type")

ggplot(trips, aes(x = trip_time, fill = rider_type, color = rider_type)) +
    geom_density() +
    ggtitle("Density Plot of Trip Times by Rider Type")

# plot the total number of trips on each day in the dataset

ggplot(trips, aes(x = ymd)) +
    geom_bar() +
    ggtitle("Total Number of Trips by Day")

# plot the total number of trips (on the y axis) by age (on the x axis) and gender (indicated with color)
ggplot(trips, aes(x = age, y = ..count.., fill = gender, color = gender)) +
    geom_bar() +
    ggtitle("Total Number of Trips by Age and Gender")

# plot the ratio of male to female trips (on the y axis) by age (on the x axis)

trips_ratio <- trips %>%
    group_by(age, gender) %>%
    summarise(trip_ratio = sum(gender == "Male") / sum(gender == "Female"))

ggplot(trips_ratio, aes(x = age, y = trip_ratio)) +
    geom_bar(stat = "identity") +
    ggtitle("Ratio of Male to Female Trips by Age")

# plot the minimum temperature (on the y axis) over each day (on the x axis)

ggplot(weather, aes(x = ymd, y = min_temp)) +
    geom_line() +
    ggtitle("Minimum Temperature by Day")

# plot the minimum temperature and maximum temperature (on the y axis, with different colors) over each day (on the x axis)
ggplot(weather, aes(x = ymd)) +

    geom_line(aes(y = min_temp), color = "blue") +
    geom_line(aes(y = max_temp), color = "red") +
    ggtitle("Minimum and Maximum Temperature by Day")

# join trips and weather
trips_with_weather <- inner_join(trips, weather, by = "ymd")


# plot the number of trips as a function of the minimum temperature, where each point represents a day
ggplot(trips_with_weather, aes(x = min_temp, y = ..count..)) +

    geom_point() +
    ggtitle("Number of Trips vs. Minimum Temperature")

# repeat this, splitting results by whether there was substantial precipitation or not
trips_with_weather <- trips_with_weather %>%

    mutate(substantial_precipitation = ifelse(precipitation > 0.5, TRUE, FALSE))

ggplot(trips_with_weather, aes(x = min_temp, y = ..count.., color = substantial_precipitation)) +

    geom_point() +
    ggtitle("Number of Trips vs. Minimum Temperature (Split by Precipitation)")

# add a smoothed fit on top of the previous plot, using geom_smooth
ggplot(trips_with_weather, aes(x = min_temp, y = ..count.., color = substantial_precipitation)) +

    geom_point() +
    geom_smooth() +
    ggtitle("Number of Trips vs. Minimum Temperature (Split by Precipitation with Smoothed Fit)")

# compute the average number of trips and standard deviation in number of trips by hour of the day

trips_by_hour <- trips %>%
    mutate(hour = hour(ymd_hms(start_time))) %>%
    group_by(hour) %>%
    summarise(avg_trips = mean(trip_count), sd_trips = sd(trip_count))

# plot the above
ggplot(trips_by_hour, aes(x = hour, y = avg_trips, ymin = avg_trips - sd_trips, ymax = avg_trips + sd_trips)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    ggtitle("Average Number of Trips by Hour of the Day")

# repeat this, but now split the results by day of the week (Monday, Tuesday, ...) or weekday vs. weekend days
trips_by_hour_weekday <- trips %>%
    mutate(weekday = wday(ymd_hms(start_time), label = TRUE)) %>%
    group_by(hour, weekday) %>%
    summarise(avg_trips = mean(trip_count), sd_trips = sd(trip_count))

ggplot(trips_by_hour_weekday, aes(x = hour, y = avg_trips, ymin = avg_trips - sd_trips, ymax = avg_trips + sd_trips, color = weekday)) +
    geom_line() +
    geom_ribbon(alpha = 0.3) +
    ggtitle("Average Number of Trips by Hour of the Day (Split by Weekday)")
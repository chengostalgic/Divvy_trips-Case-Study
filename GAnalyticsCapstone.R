library(tidyverse)
library(ggplot2)
library(lubridate)
setwd('Desktop/divvy_trips')

Divvy_19 <- read_csv("Divvy_2019.csv")
Divvy_20 <- read_csv("Divvy_2020.csv")

head(Divvy_19)
head(Divvy_20)

divvy_data <- rbind(Divvy_19, Divvy_20)

divvy_data %>%
  count(member_casual) %>%
  ggplot(aes(x = member_casual, y = n, fill = member_casual)) +
  geom_col() +
  labs(title = "Total Rides by User Type", x = "User Type", y = "Number of Rides") +
  theme_minimal()

divvy_data %>%
  mutate(month = lubridate::month(start_time, label = TRUE)) %>%
  count(member_casual, month) %>%
  ggplot(aes(x = month, y = n, fill = member_casual)) +
  geom_col(position = "dodge") +
  labs(title = "Monthly Rides by User Type", x = "Month", y = "Number of Rides") +
  theme_minimal()


divvy_data %>%
  # Convert "hh:mm:ss" to period, then to numeric minutes
  mutate(ride_length_min = as.numeric(hms(ride_length)) / 60) %>%
  # Optional: filter out very long rides (outliers)
  filter(ride_length_min < 60) %>%
  ggplot(aes(x = ride_length_min, fill = member_casual)) +
  geom_histogram(binwidth = 2, position = "identity", alpha = 0.5) +
  labs(
    title = "Trip Duration by User Type",
    x = "Ride Duration (minutes)",
    y = "Number of Rides",
    fill = "User Type"
  ) +
  theme_minimal()

divvy_data %>%
  mutate(
    start_time = ymd_hms(start_time),
    hour = hour(start_time)
  ) %>%
  count(member_casual, hour) %>%
  ggplot(aes(x = hour, y = n, color = member_casual)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 0:23) +
  labs(
    title = "Hourly Ride Patterns by User Type",
    x = "Hour of Day",
    y = "Number of Rides",
    color = "User Type"
  ) +
  theme_minimal()

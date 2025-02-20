# Case-Study - RStudio
# How does a bike-share navigate speedy success? Is subscription customer important? how to improve the number of them. in this case study, I try to compare two different types of users to figure it out.


install.packages('tidyverse')

library(tidyverse)

install.packages("ggplot2")

library(ggplot2)

df_2019_Q1 <- read_csv("Divvy_Trips_2019_Q1.csv")

summary(df_2019_Q1)

df_2019_Q1_clean <- na.omit(df_2019_Q1)

df_2019_Q1_clean$start_time <- as.POSIXct(df_2019_Q1_clean$start_time, format="%Y-%m-%d %H:%M:%S")

df_2019_Q1_clean$end_time <- as.POSIXct(df_2019_Q1_clean$end_time, format="%Y-%m-%d %H:%M:%S")

str(df_2019_Q1_clean)

df_2019_Q1_clean$ride_duration <- df_2019_Q1_clean$end_time - df_2019_Q1_clean$start_time

head(df_2019_Q1_clean$ride_duration)

summary(df_2019_Q1_clean$ride_duration)

summary(as.numeric(df_2019_Q1_clean$ride_duration))

df_2019_Q1_clean <- df_2019_Q1_clean %>% 
  filter(ride_duration > 1 & ride_duration <= 1440)
summary(as.numeric(df_2019_Q1_clean$ride_duration))

df_2019_Q1_clean$ride_duration <- as.numeric(df_2019_Q1_clean$ride_duration)

ggplot(df_2019_Q1_clean, aes(x = ride_duration)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Ride Duration", 
       x = "Ride Duration (minutes)", 
       y = "Number of Rides") +
  theme_minimal()

df_2019_Q1_clean$ride_duration <- as.numeric(df_2019_Q1_clean$ride_duration)

class(df_2019_Q1_clean$ride_duration)

ggplot(df_2019_Q1_clean, aes(x = ride_duration)) +
  geom_histogram(binwidth = 5, fill = "steelblue", color = "black") +
  labs(title = "Distribution of Ride Duration", 
       x = "Ride Duration (minutes)", 
       y = "Number of Rides")

summary(df_2019_Q1_clean$ride_duration)

hist(df_2019_Q1_clean$ride_duration, breaks = 30, main = "Histogram of Ride Duration", xlab = "Ride Duration in Minutes")

df_2019_Q1_clean$ride_duration_numeric <- as.numeric(df_2019_Q1_clean$ride_duration)

hist(df_2019_Q1_clean$ride_duration_numeric, breaks = 30, 
     main = "Histogram of Ride Duration", 
     xlab = "Ride Duration in Minutes", 
     col = "lightblue", 
     border = "black")

table(df_2019_Q1_clean$usertype)

df_annual <- df_2019_Q1_clean[df_2019_Q1_clean$usertype == "Subscriber", ]

df_casual <- df_2019_Q1_clean[df_2019_Q1_clean$usertype == "Customer", ]

mean_annual <- mean(df_annual$tripduration, na.rm = TRUE)

mean_casual <- mean(df_casual$tripduration, na.rm = TRUE)

total_annual <- nrow(df_annual)

total_casual <- nrow(df_casual)

table(df_annual$from_station_name)

table(df_casual$from_station_name)

table(df_annual$to_station_name)

table(df_casual$to_station_name)

df_combined <- rbind(
  data.frame(usertype = "Annual", tripduration = df_annual$tripduration),
  data.frame(usertype = "Casual", tripduration = df_casual$tripduration)
)

ggplot(df_combined, aes(x = usertype, y = tripduration)) +
  geom_boxplot() +
  labs(title = "Comparison of Trip Duration by User Type",
       x = "User Type", y = "Trip Duration (in seconds)")

ggplot(df_combined, aes(x = usertype)) +
  geom_bar() +
  labs(title = "Number of Trips by User Type",
       x = "User Type", y = "Number of Trips")

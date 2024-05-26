# Load the data
data <- read.csv("./activity_unzipped/activity.csv")

# Check the structure of the dataset
str(data)

# Convert the date column to Date type
data$date <- as.Date(data$date, format = "%Y-%m-%d")

# Calculate the total number of steps taken per day
total_steps_per_day <- aggregate(steps ~ date, data, sum, na.rm = TRUE)

head(total_steps_per_day)

# Make a histogram of the total number of steps taken each day
hist(total_steps_per_day$steps, main = "Total Steps per Day", xlab = "Steps", col = "blue")

# Calculate and report the mean and median of the total number of steps taken per day
mean_steps_per_day <- mean(total_steps_per_day$steps)
median_steps_per_day <- median(total_steps_per_day$steps)

mean_steps_per_day
median_steps_per_day

# Calculate the average number of steps taken in each 5-minute interval across all days
average_steps_per_interval <- aggregate(steps ~ interval, data, mean, na.rm = TRUE)

# Make a time series plot
plot(average_steps_per_interval$interval, average_steps_per_interval$steps, type = "l", 
     main = "Average Daily Activity Pattern", xlab = "5-minute Interval", ylab = "Average Number of Steps", col = "green")

# Which 5-minute interval contains the maximum number of steps on average?
max_interval <- average_steps_per_interval[which.max(average_steps_per_interval$steps), ]

max_interval

# Calculate the total number of missing values in the dataset
total_na <- sum(is.na(data$steps))

total_na

# Impute missing values using the mean for that 5-minute interval
imputed_data <- data
for (i in 1:nrow(imputed_data)) {
  if (is.na(imputed_data$steps[i])) {
    interval <- imputed_data$interval[i]
    imputed_data$steps[i] <- average_steps_per_interval[average_steps_per_interval$interval == interval, "steps"]
  }
}

# Verify no missing values remain
sum(is.na(imputed_data$steps))

# Calculate the total number of steps taken per day
total_steps_per_day_imputedData <- aggregate(steps ~ date, imputed_data, sum, na.rm = TRUE)

# Make a histogram of the total number of steps taken each day
hist(total_steps_per_day_imputedData$steps, main = "Total Steps per Day", xlab = "Steps", col = "blue")

# Calculate and report the mean and median of the total number of steps taken per day
mean_steps_per_day_imputedData <- mean(total_steps_per_day_imputedData$steps)
median_steps_per_day_imputedData <- median(total_steps_per_day_imputedData$steps)

mean_steps_per_day_imputedData
median_steps_per_day_imputedData

# Create a new factor variable for weekday and weekend
imputed_data$day_type <- ifelse(weekdays(imputed_data$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
imputed_data$day_type <- as.factor(imputed_data$day_type)

head(imputed_data)

# Calculate the average number of steps taken in each 5-minute interval across all weekdays or weekends
average_steps_weekday_weekend <- aggregate(steps ~ interval + day_type, imputed_data, mean)

# Make a panel plot
library(ggplot2)
WvW <- ggplot(average_steps_weekday_weekend, aes(x = interval, y = steps, color = day_type)) +
  geom_line() +
  facet_wrap(~day_type, ncol = 1) +
  labs(title = "Average Daily Activity Pattern: Weekdays vs Weekends", x = "5-minute Interval", y = "Average Number of Steps")
WvW


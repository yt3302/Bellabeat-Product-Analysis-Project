### 1. What are some trends in smart device usage?
### 2. How could these trends apply to Bellabeat customers?
### 3. How could these trends help infuence Bellabeat marketing strategy?


## Prepare
# Load library
library(tidyverse)
library(lubridate)
library(dplyr)
library(tidyr)
library(ggplot2)

# Set new working directory (another way to import data)
# setwd("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 2/Raw Data")
# Check if files exist
# file_list <- list.files(pattern = "*.csv")
# print(file_list)

# Import data
daily_activity <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 2/Raw Data/dailyActivity_merged.csv")
daily_calories <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 2/Raw Data/dailyCalories_merged.csv")
daily_intensities <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 2/Raw Data/dailyIntensities_merged.csv")
hourly_intensities <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 2/Raw Data/hourlyIntensities_merged.csv")
sleep_day <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 2/Raw Data/sleepDay_merged.csv")
weight_info <- read.csv("/Users/yitingcheng/Documents/Certificates/Google/Basic/8. Case Study/Case Study 2/Raw Data/weightLogInfo_merged.csv")

# Check column names
colnames(daily_activity)
colnames(daily_calories)
colnames(daily_intensities)
colnames(hourly_intensities)
colnames(sleep_day)
colnames(weight_info)

# Check the structure
str(daily_activity)
str(daily_calories)
str(daily_intensities)
str(hourly_intensities)
str(sleep_day)
str(weight_info)

# View data
head(daily_activity)
head(daily_calories)
head(daily_intensities)
head(hourly_intensities)
head(sleep_day)
head(weight_info)


## Process
# Convert ActivityDate column to date format
daily_activity$ActivityDate <- as.Date(daily_activity$ActivityDate, format="%m/%d/%Y")
daily_activity$date <- format(daily_activity$ActivityDate, format = "%m/%d/%Y")

# Convert ActivityDay column to date format
daily_calories$ActivityDay <- as.Date(daily_calories$ActivityDay, format="%m/%d/%Y")

# Convert ActivityDay column to date format
daily_intensities$ActivityDay <- as.Date(daily_intensities$ActivityDay, format="%m/%d/%Y")

# Convert the ActivityHour column to datetime format
hourly_intensities$ActivityHour <- as.POSIXct(hourly_intensities$ActivityHour, format ="%m/%d/%Y %I:%M:%S %p")
# Extract to date and hour
hourly_intensities$Date <- format(hourly_intensities$ActivityHour, format = "%m/%d/%Y")
hourly_intensities$Time <- format(hourly_intensities$ActivityHour, format = "%H:%M:%S")
# View dataset
head(hourly_intensities)

# Convert the SleepDay column to datetime format
sleep_day$SleepDay <- as.POSIXct(sleep_day$SleepDay, format ="%m/%d/%Y %I:%M:%S %p")
# Extract to date
sleep_day$date <- format(sleep_day$SleepDay, format = "%m/%d/%Y")
# View dataset
head(sleep_day)

# Convert the Date column to datetime format
weight_info$Date <- as.POSIXct(weight_info$Date, format ="%m/%d/%Y %I:%M:%S %p")
# Extract to date
weight_info$date <- format(weight_info$Date, format = "%m/%d/%Y")
# View dataset
head(weight_info)

# Check duplicated values
sum(duplicated(daily_activity))
sum(duplicated(daily_calories))
sum(duplicated(daily_intensities))
sum(duplicated(hourly_intensities))
sum(duplicated(sleep_day))
sum(duplicated(weight_info))

# Check null values
sum(is.na(daily_activity))
sum(is.na(daily_calories))
sum(is.na(daily_intensities))
sum(is.na(hourly_intensities))
sum(is.na(sleep_day))
sum(is.na(weight_info))

# Delete Fat column as most of the records is NA
weight_info <- weight_info %>%
  select(-Fat)

# Drop duplicate and null values
daily_activity <- daily_activity %>%
  distinct() %>%
  drop_na()

daily_calories <- daily_calories %>%
  distinct() %>%
  drop_na()

daily_intensities <- daily_intensities %>%
  distinct() %>%
  drop_na()

hourly_intensities <- hourly_intensities %>%
  distinct() %>%
  drop_na()

sleep_day <- sleep_day %>%
  distinct() %>%
  drop_na()

weight_info <- weight_info %>%
  distinct() %>%
  drop_na()


## Analysis
# Check how many ids 
n_distinct(daily_activity$Id)
n_distinct(daily_calories$Id)
n_distinct(daily_intensities$Id)
n_distinct(hourly_intensities$Id)
n_distinct(sleep_day$Id)
n_distinct(weight_info$Id)

# daily_activity summary
daily_activity %>%  
  select(TotalSteps,
         TotalDistance,
         SedentaryMinutes, 
         Calories) %>%
  summary()

# daily_intensities summary
daily_intensities %>%
  select(VeryActiveMinutes,
         FairlyActiveMinutes,
         LightlyActiveMinutes,
         SedentaryMinutes) %>%
  summary()

# hourly_intensities summary
hourly_intensities %>%
  select(TotalIntensity,
         AverageIntensity) %>%
  summary()

# daily_calories summary
daily_calories %>%
  select(Calories) %>%
  summary()

# sleep_day summary
sleep_day %>%
  select(TotalSleepRecords,
         TotalMinutesAsleep,
         TotalTimeInBed) %>%
  summary()

# weight_info summary 
weight_info %>%
  select(WeightKg,
         BMI) %>%
  summary()

# Outer join sleep_day and daily_activity
merged_data <- merge(sleep_day, daily_activity, by=c('Id', 'date'))
head(merged_data)

# Visualization
ggplot(data=daily_activity, aes(x=TotalSteps, y=Calories)) + 
  geom_point() + geom_smooth() + labs(title="Total Steps vs. Calories")

ggplot(data=daily_activity, aes(x=TotalSteps, y=SedentaryMinutes)) + 
  geom_point() + geom_smooth() + labs(title = "Total Steps vs. Sedentary Minutes")

ggplot(data=sleep_day, aes(x=TotalMinutesAsleep, y=TotalTimeInBed)) + 
  geom_point()+ geom_smooth() + labs(title="Total Minutes Asleep vs. Total Time in Bed")

ggplot(data=merged_data, aes(x=TotalMinutesAsleep, y=SedentaryMinutes)) + 
  geom_point() + geom_smooth() + labs(title="Minutes Asleep vs. Sedentary Minutes")

grouped_hourly_intensities <- hourly_intensities %>%
  group_by(Time) %>%
  drop_na() %>%
  summarise(average_total_intensity = mean(TotalIntensity))

ggplot(data=grouped_hourly_intensities, aes(x=Time, y=average_total_intensity)) + 
  geom_histogram(stat = "identity", fill='darkblue') +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(title="Average Total Intensity vs. Time")


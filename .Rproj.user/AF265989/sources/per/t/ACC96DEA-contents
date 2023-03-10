 # Google Capstone Study: Bellabeat.



# Load packages
# install.packages("gsubfn")
#install.packages("rmarkdown")
library(tidyverse)
library(lubridate)
library(gsubfn)
library(ggExtra)


# Load and data
sleep_day <- read.csv("sleepDay_merged.csv")
daily_activity <- read.csv("dailyActivity_merged.csv")
daily_calories <- read.csv("dailyCalories_merged.csv")
weight <- read.csv("WeightLogInfo_merged.csv")

# View data and data structure
View(sleep_day)
str(sleep_day)
View(daily_activity)
str(daily_activity)
view(daily_calories)
str(Daily_calories)
view(weight)
str(weight)






# Drop duplicate and NA rows
sleep_day_clean <- sleep_day[!duplicated(sleep_day), ] %>%
  na.omit()

daily_activity_clean <- daily_activity[!duplicated(daily_activity), ] %>%
  na.omit()

daily_calories_clean <- daily_calories[!duplicated(daily_calories), ] %>%
  na.omit()

weight_clean <- weight[!duplicated(weight), ]

# Explore The data sets
daily_activity_clean %>%
  count(Id)

sleep_day_clean %>%
  count(Id)

daily_calories_clean %>% count(Id)

# Trim sleepDay column in sleep_day dataset
sleep_day_clean$SleepDay <- gsub(
  " 12:00:00 AM", "",
  as.character(sleep_day_clean$SleepDay)
)



# Create Weekday variable for daily activities and sleep day
daily_activity_clean <- daily_activity_clean %>%
  mutate(Weekday = weekdays
  (as.Date(ActivityDate, "%m/%d/%Y")))
glimpse(daily_activity_clean)


sleep_day_clean <- sleep_day_clean %>%
  mutate(Weekday = weekdays
  (as.Date(SleepDay, "%m/%d/%Y")))
glimpse(sleep_day_clean)

daily_calories_clean <- daily_calories_clean %>%
  mutate(Weekday = weekdays
  (as.Date(ActivityDay, "%m/%d/%Y")))
glimpse(daily_calories_clean)
# Create new variable with hours instead of minutes
updated_sleep_day <- sleep_day_clean %>%
  mutate(
    Total_hours_asleep = TotalMinutesAsleep / 60,
    Total_hours_in_bed = TotalTimeInBed / 60
  )

updated_sleep_day$Total_hours_in_bed <-
  round(updated_sleep_day$Total_hours_in_bed, digits = 2)

updated_sleep_day$Total_hours_asleep <-
  round(updated_sleep_day$Total_hours_asleep, digits = 2)




# Visualize the data



# Plot calories against total steps
ggplot(
  data = daily_activity_clean,
  mapping = aes(x = TotalSteps, y = Calories)
) +
  geom_point(aes(color = Calories)) +
  geom_smooth()


# Plot Total Hours Asleep Against Weekday.
ggplot(
  data = updated_sleep_day,
  mapping = aes(
    x = Weekday,
    y = Total_hours_asleep
  )
) +
  geom_boxplot() +
  facet_wrap(~TotalSleepRecords) +
  ggExtra::rotateTextX()

# Plot Weekday with barchart

updated_sleep_day %>%
  ggplot(aes(Weekday, fill = Total_hours_asleep)) +
  geom_bar()

# Plot daily calories against weekday

daily_calories_clean %>%
  ggplot(aes(Weekday, Calories, color = Calories)) +
  geom_boxplot()


# Combine sleep day and Activity date

# Rename variables to match
Sleep_data <- updated_sleep_day %>%
  rename(Date = SleepDay)

Daily_activity_data <- daily_activity_clean %>%
  rename(Date = ActivityDate)

# Merge datasets
merged_data <- merge(Daily_activity_data, Sleep_data, by = c("Id", "Date"))

# Visualize merged data

merged_data %>%
  ggplot(aes(TotalDistance, Total_hours_asleep, color = "Blue")) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~TotalSleepRecords)


merged_data %>%
  ggplot(aes(TotalSteps, Total_hours_in_bed)) +
  geom_point(color = "Blue", alpha = 0.5) +
  geom_smooth() +
  facet_wrap(~TotalSleepRecords)

merged_data %>%
  ggplot(aes(Total_hours_asleep, Calories)) +
  geom_point() +
  geom_smooth() +
  facet_wrap(~TotalSleepRecords)

#Prepare the weights and merged data columns

weight_clean$Date1 = str_sub(weight_clean$Date,1,8)

merged_data_conv <- merged_data %>%
  rename(Date1 = Date)

#Merge second time
merged_data2 <- merge(merged_data_conv, weight_clean, by = c("Id", "Date1"))


#Plot for merged_data2

merged_data2 %>% 
  ggplot(aes(TotalSteps, WeightPounds))+
  geom_point()+
  geom_smooth()

merged_data2 %>% 
  ggplot(aes(Calories, WeightPounds))+
  geom_point()+
  geom_smooth()

merged_data2 %>% 
  ggplot(aes(TotalSteps, BMI))+
  geom_point()+
  geom_smooth()

merged_data2 %>% 
  ggplot(aes(WeightPounds, Calories))+
  geom_point()+
  geom_smooth()

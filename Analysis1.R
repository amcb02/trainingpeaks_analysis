library(tidyverse)
library(chron)
library(clock)
library(cowplot)
library(RColorBrewer)
library(ggpubr)
library(scales)
library(lubridate)
library(remotes)
remotes::install_github("grimbough/FITfileR")
library(FITfileR)
install.packages("leaflet")
library(leaflet)

workouts <- read_csv("/Users/andrewmcburney/Desktop/workouts 2.csv")%>%
  select(-c(c("HRZone6Minutes":"HRZone10Minutes"),
            c("TorqueAverage":"TorqueMax"),
            c("PWRZone7Minutes":"PWRZone10Minutes")))%>%
  mutate(completed = as.logical(case_when(
    is.na(TimeTotalInHours) == F ~ T,
    is.na(TimeTotalInHours) == T ~ F)))

bikes <- workouts%>%
  filter(WorkoutType == "Bike")

runs <- workouts%>%
  filter(WorkoutType == "Run")

swims <- workouts%>%
  filter(WorkoutType == "Swim")

#Total time vs. TSS fuction
time_tss <- function(type){
  type%>%
    filter(completed == T)%>%
ggplot(aes(x = TimeTotalInHours, y = TSS))+
  geom_line()+
  geom_smooth()
  }

time_tss(bikes)
time_tss(runs)
time_tss(swims)

bikes%>%
   pivot_longer(cols = c(HRZone1Minutes:HRZone5Minutes), names_to = "HRZone", values_to = "Minutes")%>%
  mutate(Hours = Minutes/60)%>%
  ggplot(aes(x = HRZone, y=Hours))+
           geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0,40,5))

runs%>%
   pivot_longer(cols = c(HRZone1Minutes:HRZone5Minutes), names_to = "HRZone", values_to = "Minutes")%>%
  mutate(Hours = Minutes/60)%>%
  ggplot(aes(x = HRZone, y=Hours))+
           geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0,40,5))

swims%>%
   pivot_longer(cols = c(HRZone1Minutes:HRZone5Minutes), names_to = "HRZone", values_to = "Minutes")%>%
  mutate(Hours = Minutes/60)%>%
  ggplot(aes(x = HRZone, y=Hours))+
           geom_bar(stat = "identity")+
  scale_y_continuous(breaks = seq(0,40,5))

##CHATGPT

# Read the data
data <- read_csv("/Users/andrewmcburney/Desktop/workouts 2.csv")%>%
  select(-c(c("HRZone6Minutes":"HRZone10Minutes"),
            c("TorqueAverage":"TorqueMax"),
            c("PWRZone7Minutes":"PWRZone10Minutes")))%>%
  mutate(completed = as.logical(case_when(
    is.na(TimeTotalInHours) == F ~ T,
    is.na(TimeTotalInHours) == T ~ F)))

# Filter data for swimming, biking, and running
filtered_data <- data %>%
  filter(WorkoutType %in% c("Swim", "Bike", "Run", "Lap Swimming", "Road Cycling", "Treadmill Running", "Open Water Swimming")) %>%
  mutate(WorkoutDay = as.Date(WorkoutDay, format = "%m/%d/%y"))

# Plot trends
ggplot(trends, aes(x = month, y = total_distance, color = WorkoutType, group = WorkoutType)) +
  geom_line() +
  labs(title = "Trends in Distance for Swimming, Biking, and Running", x = "Month", y = "Total Distance (m)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Analyze trends by month
trends <- filtered_data %>%
  group_by(WorkoutType, month = format(WorkoutDay, "%Y-%m")) %>%
  mutate(PowerMax = case_when(
    is.na(PowerMax) == T ~ 0,
    is.na(PowerMax) == F ~ PowerMax))%>%
    mutate(HeartRateMax = case_when(
    is.na(HeartRateMax) == T ~ 0,
    is.na(HeartRateMax) == F ~ HeartRateMax))%>%
      mutate(Rpe = case_when(
    is.na(Rpe) == T ~ 0,
    is.na(Rpe) == F ~ Rpe))%>%
        mutate(Feeling = case_when(
    is.na(Feeling) == T ~ 0,
    is.na(Feeling) == F ~ Feeling))%>%
  summarize(
    total_distance = sum(DistanceInMeters, na.rm = TRUE),
    avg_hr = mean(HeartRateAverage, na.rm = TRUE),
    avg_power = mean(PowerAverage, na.rm = TRUE),
    avg_cadence = mean(CadenceAverage, na.rm = TRUE),
    peak_power = max(PowerMax, na.rm = TRUE),
    peak_hr = max(HeartRateMax, na.rm = TRUE),
    RPE = max(Rpe, na.rm = TRUE),
    Feeling = max(Feeling, na.rm = TRUE),
    IF = mean(IF, na.rm = TRUE),
    TSS = mean(TSS, na.rm = TRUE)
  ) %>%
  ungroup() %>%
    mutate(intensity = case_when(
           (WorkoutType == "Bike" & avg_power >= 240 & IF >= 0.85 & avg_hr >= 160) ~ "high",
           (WorkoutType == "Bike" & avg_power >= 200 & IF >= 0.72 & avg_hr >= 140) ~ "medium",
           (WorkoutType == "Bike" & TRUE) ~ "low",
           (WorkoutType == "Run" & avg_power >= 290 & IF >= 0.85 & avg_hr >= 170) ~ "high",
           (WorkoutType == "Run" & avg_power >= 270 & IF >= 0.72 & avg_hr >= 160) ~ "medium",
           (WorkoutType == "Run" & TRUE) ~ "low",
            (WorkoutType == "Swim" & IF >= 0.85 & avg_hr >= 160) ~ "high",
           (WorkoutType == "Swim" & IF >= 0.72 & avg_hr >= 140) ~ "medium",
           (WorkoutType == "Swim" & TRUE) ~ "low",
         ))%>%
  mutate(month = as.Date(paste0(month, "-01")),
         intensity = case_when(
           (avg_power >= 200 & IF >= 0.9 & avg_hr >= 170) ~ "high",
           (avg_power >= 150 & IF >= 0.7 & avg_hr >= 150) ~ "medium",
           TRUE ~ "low"
         ),
         volume = case_when(
           total_distance >= 40000 ~ "high",
           total_distance >= 20000 & total_distance < 40000 ~ "medium",
           TRUE ~ "low"
         ))

#Analyze trends by workout
individual_trends <- filtered_data %>%
  group_by(WorkoutType, month = format(WorkoutDay, "%Y-%m")) %>%
  mutate(PowerMax = case_when(
    is.na(PowerMax) == T ~ 0,
    is.na(PowerMax) == F ~ PowerMax))%>%
    mutate(HeartRateMax = case_when(
    is.na(HeartRateMax) == T ~ 0,
    is.na(HeartRateMax) == F ~ HeartRateMax))%>%
      mutate(Rpe = case_when(
    is.na(Rpe) == T ~ 0,
    is.na(Rpe) == F ~ Rpe))%>%
        mutate(Feeling = case_when(
    is.na(Feeling) == T ~ 0,
    is.na(Feeling) == F ~ Feeling))%>%
  ungroup() %>%
    mutate(intensity = case_when(
           (WorkoutType == "Bike" & PowerAverage >= 220 & IF >= 0.85 & HeartRateMax >= 170) ~ "high",
           (WorkoutType == "Bike" & PowerAverage >= 200 & IF >= 0.72 & HeartRateMax >= 160) ~ "medium",
           (WorkoutType == "Bike" & TRUE) ~ "low",
           (WorkoutType == "Run" & PowerAverage >= 290 & IF >= 0.85 & HeartRateMax >= 170) ~ "high",
           (WorkoutType == "Run" & PowerAverage >= 270 & IF >= 0.72 & HeartRateMax >= 160) ~ "medium",
           (WorkoutType == "Run" & TRUE) ~ "low",
            (WorkoutType == "Swim" & IF >= 0.85 & HeartRateMax >= 160) ~ "high",
           (WorkoutType == "Swim" & IF >= 0.72 & HeartRateMax >= 140) ~ "medium",
           (WorkoutType == "Swim" & TRUE) ~ "low",
         ))

#high intensity
high_intensity <- individual_trends%>%
filter(intensity == "high")%>%
select(WorkoutType, PowerAverage, PowerMax, TimeTotalInHours, HeartRateAverage, HeartRateMax, IF, TSS, Rpe, Feeling, intensity)

#medium intensity
medium_intensity <- individual_trends%>%
filter(intensity == "medium")%>%
select(WorkoutType, PowerAverage, PowerMax, TimeTotalInHours, HeartRateAverage, HeartRateMax, IF, TSS, Rpe, Feeling, intensity)

#low intensity
low_intensity <- individual_trends%>%
filter(intensity == "low")%>%
select(WorkoutType, PowerAverage, PowerMax, TimeTotalInHours, HeartRateAverage, HeartRateMax, IF, TSS, Rpe, Feeling, intensity)

# Create separate plots for each metric
ggplot(trends, aes(x = month, y = peak_power)) +
  geom_line(aes(color = WorkoutType, group = WorkoutType)) +
  labs(title = "Trends in Peak Power", x = "Month", y = "Peak Power (W)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(trends, aes(x = month, y = peak_hr)) +
  geom_line(aes(color = WorkoutType, group = WorkoutType)) +
  labs(title = "Trends in Peak Heart Rate", x = "Month", y = "Peak Heart Rate (bpm)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(trends%>%filter(RPE != 0), aes(x = month, y = RPE)) +
  geom_line(aes(color = WorkoutType, group = WorkoutType)) +
  labs(title = "Trends in RPE", x = "Month", y = "RPE") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(trends%>%filter(Feeling !=0), aes(x = month, y = Feeling)) +
  geom_line(aes(color = WorkoutType, group = WorkoutType)) +
  labs(title = "Trends in Feeling", x = "Month", y = "Feeling")+
    theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

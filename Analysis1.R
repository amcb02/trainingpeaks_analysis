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

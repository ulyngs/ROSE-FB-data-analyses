library(tidyverse)
library(jsonlite)
library(lubridate)

#### READ IN DATA AND CALCULATE DURATIONS ####
#read in raw data
facebookData <- fromJSON("rose-data.json")

#get the window activity records
visitData <- facebookData$`window-activity-records` %>%
  flatten() %>%
  as_tibble()

#select just the parts we want
analysisData <- visitData %>%
  select(dateString, value.open, value.active) %>%
  mutate(dateString = ymd_hms(dateString)) %>%
  mutate(duration = NA) #initialise duration to NA

#calculate durations
#do this in the stupid way with a for loop
for (i in 1:(nrow(analysisData)-1)) {
  #print(str_c("jeg tjekker række nummer",i))
  analysisData[[i, "duration"]] <- ifelse(analysisData[[i, "value.active"]] == TRUE, 
                             as.period(interval(analysisData[[i,"dateString"]],analysisData[[i+1,"dateString"]])), 
                             NA)
}

#### STATS AND VISUALISATIONS ####
### Number of visits
# number of visits in total
total_visits <- analysisData %>%
  filter(value.active == TRUE) %>%
  count()

# number of visits per day
visits_per_day <- analysisData %>%
  group_by(day = floor_date(dateString, "day")) %>%
  summarise(num_visits = n())

visits_per_day %>%
  ggplot(aes(x = day, y = num_visits)) +
    geom_point() + 
    geom_line()

visits_per_day %>%
  ggplot(aes(x = day, y = num_visits)) +
  geom_bar(stat = "identity")

### time spent
# time spent in total (seconds)
analysisData %>%
  filter(value.active == TRUE) %>%
  summarise(total_duration = sum(duration) %>% seconds_to_period() %>% round())

# time spent per day (seconds)
time_per_day <- analysisData %>%
  group_by(day = floor_date(dateString, "day")) %>%
  filter(value.active == TRUE) %>%
  summarise(total_duration = sum(duration)) %>%
  mutate(period = seconds_to_period(total_duration) %>% round()) %>%
  mutate(short_date = str_c(year(day), "/", month(day), "/", day(day)))

time_per_day %>%
  ggplot(aes(x = short_date, y = total_duration)) +
    geom_bar(stat = "identity") + 
    scale_y_continuous(breaks=seq(0,1000,60)) +
    coord_flip()

# average duration per visit
help("lubridate-package")


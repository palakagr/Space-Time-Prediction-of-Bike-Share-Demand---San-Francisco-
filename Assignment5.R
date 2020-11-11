## Setup
library(tidyverse)
library(sf)
library(lubridate)
library(tigris)
library(tidycensus)
library(viridis)
library(riem)
library(gridExtra)
library(knitr)
library(kableExtra)
library(RSocrata)

mapTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 24,colour = "black"),
    plot.subtitle=element_text(face="italic"),
    plot.caption=element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),axis.title = element_blank(),
    axis.text = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2)
  )
}

plotTheme <- function(base_size = 12) {
  theme(
    text = element_text( color = "black"),
    plot.title = element_text(size = 24,colour = "black"),
    plot.subtitle = element_text(face="italic"),
    plot.caption = element_text(hjust=0),
    axis.ticks = element_blank(),
    panel.background = element_blank(),
    panel.grid.major = element_line("grey60", size = 0.1),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(colour = "black", fill=NA, size=2),
    strip.background = element_rect(fill = "grey60", color = "white"),
    strip.text = element_text(size=12),
    axis.title = element_text(size=12),
    axis.text = element_text(size=10),
    plot.background = element_blank(),
    legend.background = element_blank(),
    legend.title = element_text(colour = "black", face = "italic"),
    legend.text = element_text(colour = "black", face = "italic"),
    strip.text.x = element_text(size = 14)
  )
}

# Load Quantile break functions

qBr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

q5 <- function(variable) {as.factor(ntile(variable, 5))}

palette5 <- c("#eff3ff","#bdd7e7","#6baed6","#3182bd","#08519c")
palette4 <- c("#D2FBD4","#92BCAB","#527D82","#123F5A")
palette2 <- c("#6baed6","#08519c")

## Read data
sf_bike1 <- read.csv('Data/201803-fordgobike.csv')
sf_bike2 <- read.csv('Data/201804-fordgobike.csv')

sf_bike <- rbind(sf_bike1, sf_bike2)
glimpse(sf_bike)

sfCounty <-
  st_read("https://data.sfgov.org/api/geospatial/p5b7-5n3h?method=export&format=GeoJSON") %>% 
  st_union() %>%
  st_transform('ESRI:102241')

neighborhoods <- 
  st_read("https://data.sfgov.org/api/geospatial/pty2-tcw4?method=export&format=GeoJSON") %>%
  st_transform('ESRI:102241') %>%
  dplyr::select(-link)

# Install Census API Key
census_api_key("aea3dee2d96acb5101e94f3dcfa1b575f73d093a", overwrite = TRUE)

sfCensus <- 
  get_acs(geography = "tract", 
          variables = c("B01003_001", "B19013_001", 
                        "B02001_002", "B08013_001",
                        "B08012_001", "B08301_001", 
                        "B08301_010", "B01002_001"), 
          year = 2018, 
          state = 06, 
          geometry = TRUE, 
          county=c(001,075,081,013,085),
          output = "wide") %>%
  rename(Total_Pop =  B01003_001E,
         Med_Inc = B19013_001E,
         Med_Age = B01002_001E,
         White_Pop = B02001_002E,
         Travel_Time = B08013_001E,
         Num_Commuters = B08012_001E,
         Means_of_Transport = B08301_001E,
         Total_Public_Trans = B08301_010E) %>%
  select(Total_Pop, Med_Inc, White_Pop, Travel_Time,
         Means_of_Transport, Total_Public_Trans,
         Med_Age,
         GEOID, geometry) %>%
  mutate(Percent_White = White_Pop / Total_Pop,
         Mean_Commute_Time = Travel_Time / Total_Public_Trans,
         Percent_Taking_Public_Trans = Total_Public_Trans / Means_of_Transport) %>%
  st_transform('ESRI:102241')

sfTracts <- 
  sfCensus %>%
  as.data.frame() %>%
  distinct(GEOID, .keep_all = TRUE) %>%
  select(GEOID, geometry) %>% 
  st_sf

ggplot() +
  geom_sf(data = sfTracts)

## Creating time interval
sfBike <- sf_bike %>%
  mutate(interval60 = floor_date(ymd_hms(start_time), unit = "hour"),
         interval15 = floor_date(ymd_hms(start_time), unit = "15 mins"),
         week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  filter(week < 15)

glimpse(sfBike)

## Adding census data to bike data
sfBike_census <- st_join(sfBike %>% 
                        filter(is.na(start_station_longitude) == FALSE &
                                 is.na(start_station_latitude) == FALSE &
                                 is.na(end_station_latitude) == FALSE  &
                                 is.na(end_station_longitude) == FALSE) %>%
                        st_as_sf(., coords = c("start_station_longitude", "start_station_latitude"), crs = 4326),
                      sfTracts %>%
                        st_transform(crs=4326),
                      join=st_intersects,
                          left = TRUE) %>%
  rename(Origin.Tract = GEOID) %>%
  mutate(start_station_longitude = unlist(map(geometry, 1)),
         start_station_latitude = unlist(map(geometry, 2)))%>%
  as.data.frame() %>%
  select(-geometry)%>%
  st_as_sf(., coords = c("end_station_longitude", "end_station_latitude"), crs = 4326) %>%
  st_join(., sfTracts %>%
            st_transform(crs=4326),
          join=st_intersects,
          left = TRUE) %>%
  rename(Destination.Tract = GEOID)  %>%
  mutate(end_station_longitude = unlist(map(geometry, 1)),
         end_station_latitude = unlist(map(geometry, 2)))%>%
  as.data.frame() %>%
  select(-geometry)

## Weather Data

weather.Panel <- 
  riem_measures(station = "SFO", date_start = "2018-03-01", date_end = "2018-04-30") %>%
  dplyr::select(valid, tmpf, p01i, sknt)%>%
  replace(is.na(.), 0) %>%
  mutate(interval60 = ymd_h(substr(valid,1,13))) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label=TRUE)) %>%
  group_by(interval60) %>%
  summarize(Temperature = max(tmpf),
            Precipitation = sum(p01i),
            Wind_Speed = max(sknt)) %>%
  mutate(Temperature = ifelse(Temperature == 0, 42, Temperature))

glimpse(weather.Panel)

grid.arrange(
  ggplot(weather.Panel, aes(interval60,Precipitation)) + geom_line() + 
    labs(title="Percipitation", x="Hour", y="Perecipitation") + plotTheme,
  ggplot(weather.Panel, aes(interval60,Wind_Speed)) + geom_line() + 
    labs(title="Wind Speed", x="Hour", y="Wind Speed") + plotTheme,
  ggplot(weather.Panel, aes(interval60,Temperature)) + geom_line() + 
    labs(title="Temperature", x="Hour", y="Temperature") + plotTheme,
  top="Weather Data - San Francisco SFO - March-April, 2018")

## Exploratory analysis 

ggplot(sfBike_census %>%
         group_by(interval60) %>%
         tally())+
  geom_line(aes(x = interval60, y = n))+
  labs(title="Bike share trips per hr. Bay Area, March, 2018",
       x="Date", 
       y="Number of trips")+
  plotTheme

sfBike_census %>%
  mutate(time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                 hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                 hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                 hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush"))%>%
  group_by(interval60, start_station_name, time_of_day) %>%
  tally()%>%
  group_by(start_station_name, time_of_day)%>%
  summarize(mean_trips = mean(n))%>%
  ggplot()+
  geom_histogram(aes(mean_trips), binwidth = 1)+
  labs(title="Mean Number of Hourly Trips Per Station. SF, March-April, 2018",
       x="Number of trips", 
       y="Frequency")+
  facet_wrap(~time_of_day)+
  plotTheme

ggplot(sfBike_census %>%
         group_by(interval60, start_station_name) %>%
         tally())+
  geom_histogram(aes(n), binwidth = 5)+
  labs(title="Bike share trips per hr by station. SF, March-April, 2018",
       x="Trip Counts", 
       y="Number of Stations")+
  plotTheme


ggplot(sfBike_census %>% mutate(hour = hour(start_time)))+
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="Bike share trips in Chicago, by day of the week, May, 2018",
       x="Hour", 
       y="Trip Counts")+
  plotTheme


ggplot(sfBike_census %>% 
         mutate(hour = hour(start_time),
                weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday")))+
  geom_freqpoly(aes(hour, color = weekend), binwidth = 1)+
  labs(title="Bike share trips in Chicago - weekend vs weekday, May, 2018",
       x="Hour", 
       y="Trip Counts")+
  plotTheme

ggplot()+
  geom_sf(data = sfTracts %>%
            st_transform(crs=4326))+
  geom_point(data = sfBike_census %>% 
               mutate(hour = hour(start_time),
                      weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday"),
                      time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                              hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                              hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                              hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush"))%>%
               group_by(start_station_id, start_station_latitude, start_station_longitude, weekend, time_of_day) %>%
               tally(),
             aes(x=start_station_longitude, y = start_station_latitude, color = n), 
             fill = "transparent", alpha = 0.4, size = 0.3)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  ylim(min(sfBike_census$start_station_latitude), max(sfBike_census$start_station_latitude))+
  xlim(min(sfBike_census$start_station_longitude), max(sfBike_census$start_station_longitude))+
  facet_grid(weekend ~ time_of_day)+
  labs(title="Bike share trips per hr by station. Chicago, May, 2018")+
  mapTheme

## Space-time series
length(unique(sfBike_census$interval60)) * length(unique(sfBike_census$start_station_id))

study.panel <- 
  expand.grid(interval60=unique(sfBike_census$interval60), 
              start_station_id = unique(sfBike_census$start_station_id)) %>%
  left_join(., sfBike_census %>%
              select(start_station_id, start_station_name, Origin.Tract, start_station_longitude, start_station_latitude )%>%
              distinct() %>%
              group_by(start_station_id) %>%
              slice(1))

nrow(study.panel)      

## Ride Panel
ride.panel <- 
  sfBike_census %>%
  mutate(Trip_Counter = 1) %>%
  right_join(study.panel) %>% 
  group_by(interval60, start_station_id, start_station_name, Origin.Tract, start_station_longitude, start_station_latitude) %>%
  summarize(Trip_Count = sum(Trip_Counter, na.rm=T)) %>%
  left_join(weather.Panel) %>%
  ungroup() %>%
  filter(is.na(start_station_id) == FALSE) %>%
  mutate(week = week(interval60),
         dotw = wday(interval60, label = TRUE)) %>%
  filter(is.na(Origin.Tract) == FALSE)

ride.panel <- 
  left_join(ride.panel, sfCensus %>%
              as.data.frame() %>%
              select(-geometry), by = c("Origin.Tract" = "GEOID"))

## Time Lag
ride.panel <- 
  ride.panel %>% 
  arrange(start_station_id, interval60) %>% 
  mutate(lagHour = dplyr::lag(Trip_Count,1),
         lag2Hours = dplyr::lag(Trip_Count,2),
         lag3Hours = dplyr::lag(Trip_Count,3),
         lag4Hours = dplyr::lag(Trip_Count,4),
         lag12Hours = dplyr::lag(Trip_Count,12),
         lag1day = dplyr::lag(Trip_Count,24)) %>%
  mutate(day = yday(interval60))

as.data.frame(ride.panel) %>%
  group_by(interval60) %>% 
  summarise_at(vars(starts_with("lag"), "Trip_Count"), mean, na.rm = TRUE) %>%
  gather(Variable, Value, -interval60, -Trip_Count) %>%
  mutate(Variable = factor(Variable, levels=c("lagHour","lag2Hours","lag3Hours","lag4Hours",
                                              "lag12Hours","lag1day")))%>%
  group_by(Variable) %>%  
  summarize(correlation = round(cor(Value, Trip_Count),2))

## Regression
ride.Train <- filter(ride.panel, week <= 12)
ride.Test <- filter(ride.panel, week > 12)

### 5 regressions
reg1 <- 
  lm(Trip_Count ~  hour(interval60) + dotw + Temperature,  data=ride.Train)

reg2 <- 
  lm(Trip_Count ~  start_station_name + dotw + Temperature,  data=ride.Train)

reg3 <- 
  lm(Trip_Count ~  start_station_name + hour(interval60) + dotw + Temperature + Precipitation, 
     data=ride.Train)

reg4 <- 
  lm(Trip_Count ~  start_station_name +  hour(interval60) + dotw + Temperature + Precipitation +
       lagHour + lag2Hours +lag3Hours + lag12Hours + lag1day, 
     data=ride.Train)


## Predicting 
ride.Test.weekNest <- 
  ride.Test %>%
  nest(-week) 

model_pred <- function(dat, fit){
  pred <- predict(fit, newdata = dat)}

week_predictions <- 
  ride.Test.weekNest %>% 
  mutate(ATime_FE = map(.x = data, fit = reg1, .f = model_pred),
         BSpace_FE = map(.x = data, fit = reg2, .f = model_pred),
         CTime_Space_FE = map(.x = data, fit = reg3, .f = model_pred),
         DTime_Space_FE_timeLags = map(.x = data, fit = reg4, .f = model_pred)) %>% 
  gather(Regression, Prediction, -data, -week) %>%
  mutate(Observed = map(data, pull, Trip_Count),
         Absolute_Error = map2(Observed, Prediction,  ~ abs(.x - .y)),
         MAE = map_dbl(Absolute_Error, mean, na.rm = TRUE),
         sd_AE = map_dbl(Absolute_Error, sd, na.rm = TRUE))

week_predictions

week_predictions %>%
  dplyr::select(week, Regression, MAE) %>%
  gather(Variable, MAE, -Regression, -week) %>%
  ggplot(aes(week, MAE)) + 
  geom_bar(aes(fill = Regression), position = "dodge", stat="identity") +
  scale_fill_manual(values = palette5) +
  labs(title = "Mean Absolute Errors by model specification and week") +
  plotTheme

week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         start_station_id = map(data, pull, start_station_id)) %>%
  dplyr::select(interval60, start_station_id, Observed, Prediction, Regression) %>%
  unnest() %>%
  gather(Variable, Value, -Regression, -interval60, -start_station_id) %>%
  group_by(Regression, Variable, interval60) %>%
  summarize(Value = sum(Value)) %>%
  ggplot(aes(interval60, Value, colour=Variable)) + 
  geom_line(size = 1.1) + 
  facet_wrap(~Regression, ncol=1) +
  labs(title = "Predicted/Observed bike share time series", subtitle = "Chicago; A test set of 2 weeks",  x = "Hour", y= "Station Trips") +
  plotTheme


week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         start_station_id = map(data, pull, start_station_id), 
         start_station_latitude = map(data, pull, start_station_latitude), 
         start_station_longitude = map(data, pull, start_station_longitude)) %>%
  select(interval60, start_station_id, start_station_longitude, start_station_latitude, Observed, Prediction, Regression) %>%
  unnest() %>%
  filter(Regression == "DTime_Space_FE_timeLags") %>%
  group_by(start_station_id, start_station_longitude, start_station_latitude) %>%
  summarize(MAE = mean(abs(Observed-Prediction), na.rm = TRUE))%>%
  ggplot(.)+
  geom_sf(data = sfCensus, color = "grey", fill = "transparent")+
  geom_point(aes(x = start_station_longitude, y = start_station_latitude, color = MAE), 
             fill = "transparent", alpha = 0.4)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  ylim(min(sfBike_census$start_station_latitude), max(sfBike_census$start_station_latitude))+
  xlim(min(sfBike_census$start_station_longitude), max(sfBike_census$start_station_longitude))+
  labs(title="Mean Abs Error, Test Set, Model 4")+
  mapTheme

week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         start_station_id = map(data, pull, start_station_id), 
         start_station_latitude = map(data, pull, start_station_latitude), 
         start_station_longitude = map(data, pull, start_station_longitude),
         dotw = map(data, pull, dotw)) %>%
  select(interval60, start_station_id, start_station_longitude, 
         start_station_latitude, Observed, Prediction, Regression,
         dotw) %>%
  unnest() %>%
  filter(Regression == "DTime_Space_FE_timeLags")%>%
  mutate(weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday"),
         time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                 hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                 hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                 hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush"))%>%
  ggplot()+
  geom_point(aes(x= Observed, y = Prediction))+
  geom_smooth(aes(x= Observed, y= Prediction), method = "lm", se = FALSE, color = "red")+
  geom_abline(slope = 1, intercept = 0)+
  facet_grid(time_of_day~weekend)+
  labs(title="Observed vs Predicted",
       x="Observed trips", 
       y="Predicted trips")+
  plotTheme


week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         start_station_id = map(data, pull, start_station_id), 
         start_station_latitude = map(data, pull, start_station_latitude), 
         start_station_longitude = map(data, pull, start_station_longitude),
         dotw = map(data, pull, dotw) ) %>%
  select(interval60, start_station_id, start_station_longitude, 
         start_station_latitude, Observed, Prediction, Regression,
         dotw) %>%
  unnest() %>%
  filter(Regression == "DTime_Space_FE_timeLags")%>%
  mutate(weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday"),
         time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                 hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                 hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                 hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush")) %>%
  group_by(start_station_id, weekend, time_of_day, start_station_longitude, start_station_latitude) %>%
  summarize(MAE = mean(abs(Observed-Prediction), na.rm = TRUE))%>%
  ggplot(.)+
  geom_sf(data = sfCensus, color = "grey", fill = "transparent")+
  geom_point(aes(x = start_station_longitude, y = start_station_latitude, color = MAE), 
             fill = "transparent", size = 0.5, alpha = 0.4)+
  scale_colour_viridis(direction = -1,
                       discrete = FALSE, option = "D")+
  ylim(min(sfBike_census$start_station_latitude), max(sfBike_census$start_station_latitude))+
  xlim(min(sfBike_census$start_station_longitude), max(sfBike_census$start_station_longitude))+
  facet_grid(weekend~time_of_day)+
  labs(title="Mean Absolute Errors, Test Set")+
  mapTheme

week_predictions %>% 
  mutate(interval60 = map(data, pull, interval60),
         start_station_id = map(data, pull, start_station_id), 
         start_station_latitude = map(data, pull, start_station_latitude), 
         start_station_longitude = map(data, pull, start_station_longitude),
         dotw = map(data, pull, dotw),
         Percent_Taking_Public_Trans = map(data, pull, Percent_Taking_Public_Trans),
         Med_Inc = map(data, pull, Med_Inc),
         Percent_White = map(data, pull, Percent_White)) %>%
  select(interval60, start_station_id, start_station_longitude, 
         start_station_latitude, Observed, Prediction, Regression,
         dotw, Percent_Taking_Public_Trans, Med_Inc, Percent_White) %>%
  unnest() %>%
  filter(Regression == "DTime_Space_FE_timeLags")%>%
  mutate(weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday"),
         time_of_day = case_when(hour(interval60) < 7 | hour(interval60) > 18 ~ "Overnight",
                                 hour(interval60) >= 7 & hour(interval60) < 10 ~ "AM Rush",
                                 hour(interval60) >= 10 & hour(interval60) < 15 ~ "Mid-Day",
                                 hour(interval60) >= 15 & hour(interval60) <= 18 ~ "PM Rush")) %>%
  filter(time_of_day == "AM Rush") %>%
  group_by(start_station_id, Percent_Taking_Public_Trans, Med_Inc, Percent_White) %>%
  summarize(MAE = mean(abs(Observed-Prediction), na.rm = TRUE))%>%
  gather(-start_station_id, -MAE, key = "variable", value = "value")%>%
  ggplot(.)+
  #geom_sf(data = sfCensus, color = "grey", fill = "transparent")+
  geom_point(aes(x = value, y = MAE), alpha = 0.4)+
  geom_smooth(aes(x = value, y = MAE), method = "lm", se= FALSE)+
  facet_wrap(~variable, scales = "free")+
  labs(title="Errors as a function of socio-economic variables",
       y="Mean Absolute Error (Trips)")+
  plotTheme


## Animation

library(gganimate)
library(gifski)

week11 <-
  filter(sfBike_census , week == 11)

week11.panel <-
  expand.grid(
    interval15 = unique(week11$interval15),
    Pickup.Census.Tract = unique(sfBike_census$Origin.Tract))

ride.animation.data <-
  mutate(week11, Trip_Counter = 1) %>%
  right_join(week11.panel) %>% 
  group_by(interval15, Pickup.Census.Tract) %>%
  summarize(Trip_Count = sum(Trip_Counter, na.rm=T)) %>% 
  ungroup() %>% 
  left_join(sfTracts, by=c("Pickup.Census.Tract" = "GEOID")) %>%
  st_sf() %>%
  mutate(Trips = case_when(Trip_Count == 0 ~ "0 trips",
                           Trip_Count > 0 & Trip_Count <= 30 ~ "1-30 trips",
                           Trip_Count > 30 & Trip_Count <= 60 ~ "30-60 trips",
                           Trip_Count > 60 & Trip_Count <= 90 ~ "60-90 trips",
                           Trip_Count > 90 & Trip_Count <= 150 ~ "90-150 trips",
                           Trip_Count > 150 ~ "150+ trips")) %>%
  mutate(Trips  = fct_relevel(Trips, "0 trips","1-30 trips","30-60 trips",
                              "60-90 trips","90-150 trips","150+ trips"))

rideshare_animation <-
  ggplot() +
  geom_sf(data = ride.animation.data, aes(fill = Trips)) +
  scale_fill_manual(values = palette5) +
  labs(title = "Rideshare pickups for one week in March 2018",
       subtitle = "15 minute intervals: {current_frame}") +
  transition_manual(interval15) +
  mapTheme()

animate(rideshare_animation, duration=20, renderer = gifski_renderer())

##
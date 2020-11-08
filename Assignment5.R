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
         dotw = wday(interval60, label=TRUE))

glimpse(sfBike)

## Adding census data to bike data
sfBike_census <- st_join(sfBike %>% 
                        filter(is.na(start_station_longitude) == FALSE &
                                 is.na(start_station_latitude) == FALSE &
                                 is.na(end_station_latitude) == FALSE &
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
  select(-geometry)  %>%
  st_as_sf(., coords = c("start_station_longitude", "start_station_latitude"), crs = 4326) %>%
  st_transform('ESRI:102241')

sfBikeonly <- st_intersection(sfBike_census, st_union(neighborhoods))

ggplot()+
  geom_sf(data = sfCounty,color = "transparent")+
  geom_sf(data = sfBikeonly, show.legend = "point")

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

ggplot(sfBikeonly %>%
         group_by(interval60) %>%
         tally())+
  geom_line(aes(x = interval60, y = n))+
  labs(title="Bike share trips per hr. SF, March, 2018",
       x="Date", 
       y="Number of trips")+
  plotTheme

sfBikeonly %>%
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

ggplot(sfBikeonly %>%
         group_by(interval60, start_station_name) %>%
         tally())+
  geom_histogram(aes(n), binwidth = 5)+
  labs(title="Bike share trips per hr by station. SF, March-April, 2018",
       x="Trip Counts", 
       y="Number of Stations")+
  plotTheme


ggplot(sfBikeonly %>% mutate(hour = hour(start_time)))+
  geom_freqpoly(aes(hour, color = dotw), binwidth = 1)+
  labs(title="Bike share trips in Chicago, by day of the week, May, 2018",
       x="Hour", 
       y="Trip Counts")+
  plotTheme


ggplot(sfBikeonly %>% 
         mutate(hour = hour(start_time),
                weekend = ifelse(dotw %in% c("Sun", "Sat"), "Weekend", "Weekday")))+
  geom_freqpoly(aes(hour, color = weekend), binwidth = 1)+
  labs(title="Bike share trips in Chicago - weekend vs weekday, May, 2018",
       x="Hour", 
       y="Trip Counts")+
  plotTheme

ggplot()+
  geom_sf(data = sfCounty %>%
            st_transform(crs=4326))+
  geom_point(data = sfBikeonly %>% 
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
  ylim(min(dat_census$from_latitude), max(dat_census$from_latitude))+
  xlim(min(dat_census$from_longitude), max(dat_census$from_longitude))+
  facet_grid(weekend ~ time_of_day)+
  labs(title="Bike share trips per hr by station. Chicago, May, 2018")+
  mapTheme
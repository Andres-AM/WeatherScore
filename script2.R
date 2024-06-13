

library(openmeteo)
library(tidyverse)
library(lubridate)

code <- geocode("geneva",n_results = 10,language = "en",silent = T)

code <- code |> 
  filter(country_code == "CH")

dr <- weather_forecast(location = code$name,
                       hourly = c("temperature_2m", 
                                  "precipitation",
                                  "relative_humidity_2m",
                                  "wind_speed_10m","wind_gusts_10m",
                                  "cloud_cover"
                                  ),
                       start = today(),end = today())

df <- dr|> 
  mutate( date = date(datetime)) |> 
  group_by(date) |> 
  summarise( temp_min = min(hourly_temperature_2m), 
             temp_max = max(hourly_temperature_2m),
             wind_min = min(hourly_wind_speed_10m),
             wind_max = max(hourly_wind_speed_10m),
             hourly_precipitation = min(hourly_precipitation)
  ) 

df

dr_daily <- weather_forecast("Geneva",
                       daily = c("uv_index_max"),
                       start = today(),end = today())  


df <- df |> 
  left_join(dr_daily,by = "date")
df

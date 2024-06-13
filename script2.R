

library(openmeteo)
library(tidyverse)
library(lubridate)

code <- geocode("geneva",n_results = 10,language = "en",silent = T)

code <- code |> 
  filter(country_code == "CH")

dr <- weather_forecast(location = code$name,
                       hourly = c("temperature_2m", "precipitation",
                                  "relative_humidity_2m",
                                  "wind_speed_10m","wind_gusts_10m",
                                  "cloud_cover"
                                  ),
                       start = today(),end = today())


dr

dr_daily <- weather_forecast("Geneva",
                       daily = c("uv_index_clear_sky_max","uv_index_max"),
                       start = today(),end = today(),)

dr_daily


source("libraries.R")
library(openmeteo)

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
             wind_gusts_10m = max(hourly_wind_gusts_10m),
             hourly_precipitation = min(hourly_precipitation)
  ) 

dr_daily <- weather_forecast("Geneva",
                       daily = c("uv_index_max"),
                       start = today(),end = today())  

df <- df |> 
  left_join(dr_daily,by = "date")

df |> 
  ggplot()+
  geom_errorbar(aes(xmin = temp_min,xmax = temp_max,y = wind_max),width = 2) +
  geom_text(aes(x = temp_min, y = wind_max, label = temp_min), vjust = -4, color = "blue") +
  geom_text(aes(x = temp_max, y = wind_max, label = temp_max), vjust = -4, color = "red") +
  scale_x_continuous(n.breaks = 10,limits = c(-5,35)) +
  scale_y_continuous(n.breaks = 10,limits = c(0,40)) +
  geom_hline(yintercept = 10, linetype = 2, col = "grey")+
  geom_vline(xintercept = 0, linetype = 2, col = "grey") +
  theme(axis.text.x = element_text(angle = 0)) +
  annotate("rect", xmin = -5, xmax = 10, ymin = 5, ymax = 40, alpha = 0.2, fill = "blue")+
  labs(y = "wind", x = "temp",title  = paste0("Target"))



 
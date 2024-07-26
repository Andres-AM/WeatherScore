
source("libraries.R")
source("FUN.R")

get_temperatures <- function(location_name = location_name, 
                             n_results = n_results, 
                             language = language, 
                             silent = silent, 
                             lwr_lim  = lwr_lim, 
                             upr_lim = upr_lim ) {
  
  code <- geocode(location_name = location_name, n_results = n_results, language = language, silent = silent) |> 
    filter(country_code == "CH")
  
  all_data <- weather_forecast(location = code$name,
                         hourly = c("temperature_2m", "precipitation",
                                    "relative_humidity_2m",
                                    "wind_speed_10m","wind_gusts_10m",
                                    "cloud_cover"),
                         start = lwr_lim,
                         end = upr_lim
  )
  
  df <- all_data |> 
    mutate( date = date(datetime)) |> 
    filter( date(datetime) == today()) |> 
    group_by(date) |> 
    summarise( t_min = min(hourly_temperature_2m), 
               t_max = max(hourly_temperature_2m),
               t_delta = t_max - t_min,
               wind_min = min(hourly_wind_speed_10m),
               wind_max = max(hourly_wind_speed_10m),
               wind_gusts_10m = max(hourly_wind_gusts_10m),
               hourly_precipitation = min(hourly_precipitation)) 
  
  df_daily <- weather_forecast(code$name,
                               daily = c("uv_index_max"),
                               start = lwr_lim,end = upr_lim)  
  
  specs <- df |> 
    left_join(df_daily, by = "date")
  
  results <- list(specs = specs , all_data =  all_data )
  
  return(results)
  
}

lwr_lim = today() - 7
upr_lim = today()

df <- get_temperatures(location_name = "Geneva", n_results = 10, language = "en",silent = T, lwr_lim = lwr_lim, upr_lim = upr_lim)

lim_data <- df$all_data |>
  mutate( h = hour(datetime)) |>
  select(-datetime) |>
  dplyr::summarise(.by = h, 
                   t_lwr_lim = quantile(hourly_temperature_2m,probs = 0.1),
                   t_upr_lim = quantile(hourly_temperature_2m,probs = 0.9)) |> 
  mutate( h = today() + hours(h - 2))

df$all_data |> 
  filter( date(datetime) == today()) |> 
  ggplot(aes(datetime,hourly_temperature_2m)) + 
  geom_line(aes(color = hourly_temperature_2m ))  + 
  geom_errorbar(aes(ymin = df$specs$t_min ,ymax = df$specs$t_max, x = today() - hours(2)),width = 4) +
  annotate("rect", xmin = today() - hours(2) , xmax = now(), ymin = df$specs$t_min - 5, ymax = df$specs$t_max +5, alpha = 0.2, fill = "grey") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  scale_y_continuous(n.breaks = 10,limits = c(df$specs$t_min-5,df$specs$t_max +5)) +
  theme(legend.position = "None") + 
  geom_line( data = lim_data,aes(x = h  ,  y = t_lwr_lim ),alpha = .2)+
  geom_line( data = lim_data,aes(x = h  ,  y = t_upr_lim ),alpha = .2)


df$specs

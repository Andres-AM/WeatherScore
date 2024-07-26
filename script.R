
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
  
  lim_data <- all_data |>
    mutate( h = hour(datetime)) |>
    select(-datetime) |>
    dplyr::summarise(.by = h, 
                     t_lwr_lim = quantile(hourly_temperature_2m,probs = 0.1),
                     t_upr_lim = quantile(hourly_temperature_2m,probs = 0.9)) |> 
    mutate( h = today() + hours(h - 2))
  
  df <- all_data |> 
    mutate( date = date(datetime)) |> 
    group_by(date) |> 
    summarise( t_min_night = min(hourly_temperature_2m), 
               t_min = min( hourly_temperature_2m[hour(datetime )>= 8]),   # Values below 8 are not included
               t_max = max(hourly_temperature_2m),
               t_delta = t_max - t_min,
               wind_min = min(hourly_wind_speed_10m),
               wind_max = max(hourly_wind_speed_10m),
               wind_gusts_10m = max(hourly_wind_gusts_10m),
               hourly_precipitation = min(hourly_precipitation)) 
  
  df_daily <- weather_forecast(code$name,
                               daily = c("uv_index_max"),
                               start = lwr_lim,end = upr_lim)  
  
  all_specs <- df |> 
    left_join(df_daily, by = "date")
  
  specs <- all_specs |> 
    filter( date(date) == today()) 
  

  
  results <- list(specs = specs ,all_specs = all_specs, all_data =  all_data, lim_data = lim_data)
  
  return(results)
  
}

lwr_lim = today() - 10
upr_lim = today()
pad <- 2
location_name <-  "Geneva"

df <- get_temperatures(location_name = location_name, n_results = 10, language = "en",silent = T, lwr_lim = lwr_lim, upr_lim = upr_lim)

# lim_data <- df$all_data |>
#   mutate( h = hour(datetime)) |>
#   select(-datetime) |>
#   dplyr::summarise(.by = h, 
#                    t_lwr_lim = quantile(hourly_temperature_2m,probs = 0.1),
#                    t_upr_lim = quantile(hourly_temperature_2m,probs = 0.9)) |> 
#   mutate( h = today() + hours(h - 2))

# df$lim_data

plot_t <- df$all_data |> 
  filter( date(datetime) == today()) |> 
  ggplot(aes(datetime,hourly_temperature_2m)) + 
  geom_line(aes(color = hourly_temperature_2m )) + 
  geom_errorbar(aes(ymin = df$specs$t_min ,ymax = df$specs$t_max, x = today() - hours(2),width = 4)) +
  annotate("rect", xmin = today() - hours(2) , xmax = now(), ymin = df$specs$t_min - pad, ymax = df$specs$t_max +pad, alpha = 0.2, fill = "grey") +
  scale_x_datetime(date_breaks = "1 hours", date_labels = "%H:%M") +
  scale_y_continuous(n.breaks = 10,limits = c(df$specs$t_min-pad,df$specs$t_max +pad)) +
  theme(legend.position = "None") + 
  scale_color_gradient(low = "lightblue", high = "red",limits = c(df$specs$t_min,df$specs$t_max)) +
  geom_line( data = df$lim_data, aes(x = h  ,  y = t_lwr_lim ),alpha = .3) +
  geom_line( data = df$lim_data, aes(x = h  ,  y = t_upr_lim ),alpha = .3) +
  geom_text(x = today() - hours(2), y = df$specs$t_min, label = df$specs$t_min, vjust =  pad, color = "blue")  +
  geom_text(x = today() - hours(2), y = df$specs$t_max, label = df$specs$t_max, vjust = - pad, color = "red")  +
  geom_text(x = today() - hours(2), y = (df$specs$t_max + df$specs$t_min)/2, label = paste("d",df$specs$t_delta), hjust = -.5)  
  

plot_t
df$specs



plot_w <- df$specs |>
  ggplot()+
  geom_errorbar(aes(xmin = t_min,xmax = t_max,y = wind_max),width = 2) +
  geom_text(aes(x = t_min, y = wind_max, label = t_min), vjust = -4, color = "blue") +
  geom_text(aes(x = t_max, y = wind_max, label = t_max), vjust = -4, color = "red") +
  geom_text(aes(x =  (df$specs$t_max + df$specs$t_min)/2 , y =wind_max, label = paste("d",df$specs$t_delta)), vjust = -2) + 
  scale_x_continuous(n.breaks = 10,limits = c(-5,35)) +
  scale_y_continuous(n.breaks = 10,limits = c(0,40)) +
  geom_hline(yintercept = 10, linetype = 2, col = "grey")+
  geom_vline(xintercept = 0, linetype = 2, col = "grey") +
  theme(axis.text.x = element_text(angle = 0)) +
  annotate("rect", xmin = -5, xmax = 10, ymin = 5, ymax = 40, alpha = 0.2, fill = "blue")+
  annotate("rect", xmin = -5, xmax = 10, ymin = 5, ymax = 40, alpha = 0.2, fill = "lightblue")+
  annotate("rect", xmin = 25, xmax = 35, ymin = 0, ymax = 25, alpha = 0.2, fill = "red")+
  annotate("rect", xmin = 15, xmax = 25, ymin = 5, ymax = 35, alpha = 0.2, fill = "orange")+
  annotate("rect", xmin = 25, xmax = 35, ymin = 25, ymax = 35, alpha = 0.2, fill = "orange")+
  labs(y = "wind", x = "temp",title  = paste0("Target"))

plot_w
plot_t

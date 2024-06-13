
library(tidyverse)
library(lubridate)
library(worldmet)

dr <- importNOAA(code = "067000-99999",n.cores = 2,year = 2024)


df <- dr |> 
  select(date,air_temp) |> 
  mutate( date = lubridate::ymd_hms(date))


df |> 
  filter( month(date) == 4) |> 
  ggplot(aes(x= date, y = air_temp)) +
  geom_line() +
  scale_x_date(date_labels = "%m")
# +
  # labs(title  = paste0("Temperature for the month : ",input$month_val)) +
  # theme(legend.position = "None")

# plot <- df$specs |> 
#   ggplot()+
#   geom_errorbar(aes(xmin = t_min,xmax = t_max,y = wind_max),width = 2) +
#   geom_text(aes(x = t_min, y = wind_max, label = t_min), vjust = -4, color = "blue") +
#   geom_text(aes(x = t_max, y = wind_max, label = t_max), vjust = -4, color = "red") +
#   scale_x_continuous(n.breaks = 10,limits = c(-5,35)) +
#   scale_y_continuous(n.breaks = 10,limits = c(0,40)) +
#   geom_hline(yintercept = 10, linetype = 2, col = "grey")+
#   geom_vline(xintercept = 0, linetype = 2, col = "grey") +
#   theme(axis.text.x = element_text(angle = 0)) +
#   annotate("rect", xmin = -5, xmax = 10, ymin = 5, ymax = 40, alpha = 0.2, fill = "blue")+
#   labs(y = "wind", x = "temp",title  = paste0("Target"))
# plot
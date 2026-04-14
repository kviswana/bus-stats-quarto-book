kiosk |> 
  point_plot(customers ~ temperature) +
  geom_abline(slope = 2, intercept = 15) +
  geom_abline(slope = 1, intercept = 15) +
  geom_abline(slope = 1.42, intercept = 22.06) +
  xlim(0, NA) +
  ylim(0, NA) +
  annotate("text", x = 30, y = 90, 
           label = "Model 1") +
  annotate("text", x = 40, y = 40, 
           label = "Model 2") +
  annotate("text", x = 50, y = 80, 
           label = "Model 3") 

17+1.3*66

x0 <- 13.3
y0 <- 1170


price_demand |> 
  point_plot(sales_qty ~ price) +
  geom_segment(
    x = x0, xend = x0,
    y = 0,  yend = y0,
    linetype = "dashed",
    linewidth = 1.5
  ) +
  
  # horizontal dashed line: point → y-axis
  geom_segment(
    x = 0,   xend = x0,
    y = y0,  yend = y0,
    linetype = "dashed",
    linewidth = 1.5
  ) +
  
  # highlight the point
  geom_point(aes(x = x0, y = y0), color = "red", size = 5) 

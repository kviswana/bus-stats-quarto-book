
kiosk |> 
  model_train(customers ~ temperature) |> 
  conf_interval()

kiosk |> 
  point_plot(customers ~ temperature, annot = "model")

kiosk |> 
  point_plot(customers ~ temperature) +
  geom_abline(intercept = 15, slope = 2) +
  scale_x_continuous(limits = c(0, 85), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 150), 
                     breaks = seq(0, 150, 10),
                     expand = c(0, 0))

kiosk |> 
  point_plot(customers ~ temperature) +
  geom_abline(intercept = 15, slope = 1.5) +
  scale_x_continuous(limits = c(0, 85), expand = c(0, 0)) +
  scale_y_continuous(limits = c(0, 150), 
                     breaks = seq(0, 150, 10),
                     expand = c(0, 0))


## Plot of points, regression line and three chosen points for residuals

library(tidyverse)

# Define the fixed model
a <- 15
b <- 1.5

# Add predictions and residuals from THIS line (not lm)
k2 <- kiosk %>%
  mutate(
    y_hat = a + b * temperature,
    resid = customers - y_hat
  )

# Manually select the three points
picked <- k2 %>%
  filter(
    (temperature == 62 & customers == 105) |
      (temperature == 68 & customers == 133) |
      (temperature == 76 & customers == 130)
  ) %>%
  mutate(
    resid_lab = sprintf("%+.1f", resid),   # keep + / − sign
    label_y = (customers + y_hat) / 2       # midpoint of residual
  )

# Plot
ggplot(k2, aes(x = temperature, y = customers)) +
  geom_point(size = 2) +
  geom_abline(intercept = a, slope = b, linewidth = 1, color = "blue") +
  geom_segment(
    data = picked,
    aes(xend = temperature, yend = y_hat),
    linewidth = 0.9
  ) +
  scale_x_continuous(breaks = seq(58, 80, 1)) +
  geom_text(
    data = picked,
    aes(y = label_y, label = resid_lab),
    nudge_x = 0.6,
    hjust = 0
  ) +
  labs(
    x = "Temperature (°F)",
    y = "Customers",
    title = "Residuals relative to the model: customers = 15 + 1.5 × temperature"
  )
sum(k2$resid^2)

## LOWESS model example
mpg |> 
  ggplot(aes(displ, hwy)) + 
  geom_point() +
  geom_smooth(se = FALSE)

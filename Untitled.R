library(dplyr)
library(ggplot2)

# Choose a temperature value where the plot is visually empty
label_x <- 10

coefs <- kiosk_beach_mall_temp %>%
  group_by(kiosk) %>%
  do({
    fit <- lm(customers ~ temperature, data = .)
    tibble(
      intercept = coef(fit)[1],
      slope     = coef(fit)[2]
    )
  }) %>%
  ungroup() %>%
  mutate(
    label_x = label_x,
    label_y = intercept + (slope*2) * label_x,
    label = paste0(
      kiosk, " Intercept = ", round(intercept, 1)
    )
  )

ggplot(
  kiosk_beach_mall_temp,
  aes(temperature, customers, color = kiosk)
) +
  geom_point(alpha = 0.6) +
  
  # Extended regression lines
  geom_abline(
    data = coefs,
    aes(intercept = intercept, slope = slope, color = kiosk),
    linewidth = 1.2
  ) +
  
  # Intercept points at temperature = 0
  geom_point(
    data = coefs,
    aes(x = 0, y = intercept, color = kiosk),
    size = 3
  ) +
  
  # Labels placed in empty region (not on the intercept itself)
  geom_label(
    data = coefs,
    aes(
      x = label_x,
      y = label_y,
      label = label,
      color = kiosk
    ),
    show.legend = FALSE,
    hjust = 0,
    label.size = 0
  ) +
  
  # Force axes to show intercept region
  coord_cartesian(
    xlim = c(0, NA),
    ylim = c(-90, NA)
  ) +
  labs(
    x = "Temperature",
    y = "Customers",
    color = "Kiosk type"
  )

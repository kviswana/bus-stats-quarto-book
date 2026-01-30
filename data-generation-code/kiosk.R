library(tidyverse)

set.seed(123)   # ensures reproducibility for the book

kiosk <- tibble(
  day = paste0("D", sprintf("%02d", 1:25)),
  temperature = rep(seq(58, 80, by = 2), length.out = 25)
) |>
  mutate(
    customers = round(
      10 + 1.6 * temperature + rnorm(25, mean = 0, sd = 8)
    )
  )

kiosk


kiosk |> 
  point_plot(customers ~ temperature)

kiosk |> 
  kable()

write_csv(kiosk, "lst-extras-datasets/koisk.csv")

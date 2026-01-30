library(dplyr)

kiosk_beach <- read_csv("kiosk_beach.csv")

set.seed(42)

# 1. Fit the Beach regression (DO NOT CHANGE THE DATA)
beach_fit <- lm(customers ~ temperature, data = kiosk_beach)

b0 <- coef(beach_fit)[1]
b1 <- coef(beach_fit)[2]   # observed Beach slope

# 2. Choose where lines intersect (mid-range is safest)
pivot_temp <- median(kiosk_beach$temperature)
pivot_customers <- b0 + b1 * pivot_temp

# 3. Choose a smaller Mall slope (fraction of Beach slope)
mall_slope <- 0.45 * b1    # smaller but still clearly positive

# 4. Choose Mall noise (tight!)
sd_mall <- 2

# 5. Generate Mall customers
kiosk_mall <- kiosk_beach %>%
  transmute(
    day = day,
    kiosk = "Mall",
    temperature = temperature,
    customers = round(
      pivot_customers +
        mall_slope * (temperature - pivot_temp) +
        rnorm(n(), 0, sd_mall)
    )
  )

# 6. Combine
kiosk_beach_mall_temp <- bind_rows(kiosk_beach, kiosk_mall)

write_csv(kiosk_beach_mall_temp, "kiosk_beack_mall_temp.csv")

# --- Optional checks ---
summary(lm(customers ~ temperature,
           data = filter(kiosk_beach_mall, kiosk == "Beach")))

summary(lm(customers ~ temperature,
           data = filter(kiosk_beach_mall, kiosk == "Mall")))

kiosk_mall |> 
  point_plot(customers ~ temperature, annot = "model")

kiosk_beach |> 
  point_plot(customers ~ temperature, annot = "model")

kiosk_beach_mall_temp |> 
  point_plot(customers ~ temperature + kiosk,
             annot = "model",
             interval = "none")

kiosk_beach_mall_temp <- read_csv("kiosk_beack_mall_temp.csv")

kiosk_beach_mall_temp |> 
  point_plot(customers ~ temperature + kiosk,
             annot = "model",
             interval = "none")


kiosk_beach_mall_temp |> 
  model_train(customers ~ temperature + kiosk) |> 
  coef()

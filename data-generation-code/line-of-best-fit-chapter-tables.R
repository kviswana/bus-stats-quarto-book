kiosk_15_2 <- kiosk |> 
  mutate(est_customers = 15 + 2*temperature,
         diff = customers - est_customers,
         penalty = diff^2)

kiosk_15_2 |> kable()

sum(kiosk_15_2$penalty)


kiosk_15_1_5 <- kiosk |> 
  mutate(est_customers = 15 + 1.5*temperature,
         diff = customers - est_customers,
         penalty = diff^2)

sum(kiosk_15_1_5$penalty)

kiosk_22_06_1_42 <- kiosk |> 
  mutate(est_customers = 22.06 + 1.42*temperature,
         diff = customers - est_customers,
         penalty = diff^2)

sum(kiosk_22_06_1_42$penalty)

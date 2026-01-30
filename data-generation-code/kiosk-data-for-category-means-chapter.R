set.seed(123)  # for reproducibility

# Number of observations per kiosk
n_per_kiosk <- 10

# Create kiosk labels
kiosk <- c(
  rep("Mall", n_per_kiosk),
  rep("Beach", n_per_kiosk)
)

# Generate customer counts
customers <- c(
  round(rnorm(n_per_kiosk, mean = 100, sd = 20)),   # Mall
  round(rnorm(n_per_kiosk, mean = 140, sd = 30))    # Beach
)

# Create data frame
kiosk_data <- data.frame(
  customers = customers,
  kiosk = kiosk
)

# Display as markdown table
knitr::kable(kiosk_data)

kiosk_data |> 
  summarize(avg_customers = mean(customers), .by = kiosk) |> 
  kable()

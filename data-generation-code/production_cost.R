set.seed(123)

n <- 100

x <- rnorm(n, mean = 500, sd = 100)

units_produced <- round(x / 10) * 10
fixed_cost = 10000
# Exact linear relationship
total_cost <- fixed_cost + 45 * units_produced

production_cost <- data.frame(
  units_produced,
  total_cost
)

# Verify perfect correlation
cor(production_cost$units_produced, production_cost$total_cost)

production_cost |> 
  point_plot(total_cost ~ units_produced) +
  scale_y_continuous(limits = c(0,45000))

write_csv(production_cost, "lst-extras-datasets/production_cost.csv")

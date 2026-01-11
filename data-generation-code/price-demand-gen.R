set.seed(123)

n <- 500              # number of observations
target_r <- -0.75     # desired correlation

# Step 1: generate correlated standard normal data
price_std <- rnorm(n)
sales_std <- target_r * price_std + sqrt(1 - target_r^2) * rnorm(n)

# Step 2: rescale to desired ranges
price <- 10 + (price_std - min(price_std)) / 
  (max(price_std) - min(price_std)) * (15 - 10)

sales_qty <- 1000 + (sales_std - min(sales_std)) / 
  (max(sales_std) - min(sales_std)) * (1500 - 1000)

# Combine into a data frame
df <- data.frame(
  price = round(price,1),
  sales_qty = round(sales_qty, 0)
)

# Check correlation
cor(df$price, df$sales_qty)

library(LSTbook)
df |> 
  point_plot(sales_qty ~ price)

nrow(df)

write_csv(df, "lst-extras-datasets/price-demand.csv")

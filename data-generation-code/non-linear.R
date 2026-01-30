library(ggplot2)

set.seed(42)
n <- 250

# Symmetric x helps keep linear correlation low
x <- runif(n, -3, 3)

# Strong nonlinear relationship with noise
y <- x^2 + rnorm(n, mean = 0, sd = 0.7)

df <- data.frame(x = x, y = y)

# Check correlation (often near 0 despite strong curve)
cor(df$x, df$y)

# Plot: scattered points + underlying curve
ggplot(df, aes(x = x, y = y)) +
  geom_point(alpha = 0.6) +
  stat_function(fun = function(t) t^2, linewidth = 1) +
  labs(x = "x", y = "y")

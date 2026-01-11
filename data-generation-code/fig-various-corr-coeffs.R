library(ggplot2)
library(patchwork)

set.seed(123)

r_vals <- seq(-1, 1, by = 0.25)
n <- 200

plots <- lapply(r_vals, function(r) {
  
  x <- rnorm(n)
  y <- r * x + sqrt(1 - r^2) * rnorm(n)
  
  df <- data.frame(x, y)
  
  ggplot(df, aes(x, y)) +
    geom_point(color = "steelblue", alpha = 0.7, size = 0.2) +
    # geom_smooth(method = "lm", se = FALSE, color = "darkred") +
    labs(title = paste("r =", r), x = "X", y = "Y") +
    theme_minimal(base_size = 11)
})

wrap_plots(plots, ncol = 3)

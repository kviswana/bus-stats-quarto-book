set.seed(123)

n <-1000
lo <- 1
hi <- 100

# 0) Perfect uniform distribution
x0 <- rep(1:100, each = 10)

# 1) Nearly uniformly distributed integers
x1 <- sample(lo:hi, n, replace = TRUE)

# Helper: draw from Normal, then clamp to [lo, hi] and round to integers
rnorm_int_clamp <- function(n, mean, sd, lo = lo, hi = hi) {
  x <- rnorm(n, mean = mean, sd = sd)
  x <- pmin(pmax(x, lo), hi)      # clamp
  as.integer(round(x))
}

# 2) Normal(mean=50, sd=6), integers 1..100
x2 <- rnorm_int_clamp(n, mean = 50, sd = 8, lo = lo, hi = hi)

# 3) Normal(mean=50, sd=15), integers 1..100
x3 <- rnorm_int_clamp(n, mean = 50, sd = 15, lo = lo, hi = hi)

# 4) Bimodal: two well-separated modes near 30 and 65
#    (half from N(30,4), half from N(65,4))
x4 <- c(
  rnorm_int_clamp(n/2, mean = 30, sd = 4, lo = lo, hi = hi),
  rnorm_int_clamp(n/2, mean = 65, sd = 4, lo = lo, hi = hi)
)
x4 <- sample(x4)  # shuffle order

# Helper: Beta skew mapped to 1..100 integers
rbeta_int <- function(n, shape1, shape2, lo = lo, hi = hi) {
  x <- rbeta(n, shape1, shape2)          # in [0,1]
  as.integer(floor(lo + x * (hi - lo + 1))) |> pmin(hi) |> pmax(lo)
}

# 5) Skewed toward higher values (more mass near 100)
x5 <- rbeta_int(n, shape1 = 5, shape2 = 1, lo = lo, hi = hi)

# 6) Skewed toward lower values (more mass near 1)
x6 <- rbeta_int(n, shape1 = 2, shape2 = 8, lo = lo, hi = hi)



plot_hist_0 <- function(x) {
  df = data.frame(x)
  df %>% 
    ggplot(aes(x = x)) +
    geom_histogram(fill = "lightblue", color = "black", bins = 100) +
    scale_x_continuous(breaks = seq(1, 100, 1), limits = c(0, 101)) 
    
}

plot_hist <- function(x) {
  df <- data.frame(x)
  
  df %>%
    ggplot(aes(x = x)) +
    geom_histogram(
      binwidth = 1,
      boundary = 0.5,   # bins centered on integers: [0.5,1.5), [1.5,2.5), ...
      fill = "lightblue",
      color = "black"
    ) +
    scale_x_continuous(breaks = seq(0, 100, 2), limits = c(0, 101)) +
    scale_y_continuous(limits = c(0, 50))
}



plot_hist(x0)
plot_hist(x1)
plot_hist(x2)
plot_hist(x3)
plot_hist(x4)
plot_hist(x5)
plot_hist(x6)

mean(x0)
range(x0)

mean(x1)
range(x1)

mean(x2)
range(x2)


set.seed(123)
n <- 500
daily_return <- rnorm(n, mean = 0.0004, sd = 0.02)

dpo_raw <- rnorm(n, mean = 45, sd = 10)

# Remove any accidental linear association with daily_return
dpo <- dpo_raw - coef(lm(dpo_raw ~ daily_return))[2] * daily_return
dpo <- round(dpo, 1)

df <- data.frame(daily_return, dpo)

cor(df$daily_return, df$dpo)

write_csv(df, "lst-extras-datasets/returns-dpo.csv")


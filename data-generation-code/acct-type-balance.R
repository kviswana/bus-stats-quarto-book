n <- 50
types <- c("Checking", "Savings")
balance <- vector(mode = "integer", length = n)
bank_account_type <- factor(sample(types, n, replace = TRUE))
balance[bank_account_type == "Checking"] =
  ceiling(rnorm(sum(bank_account_type == "Checking"),
                5000, 700))

balance[bank_account_type == "Savings"] =
  ceiling(rnorm(sum(bank_account_type == "Savings"),
                6000, 600))
acct_type_balance <- data.frame(bank_account_type, balance)

write_csv(acct_type_balance, "lst-extras-datasets/acct_type_balance.csv")

acct_type_balance |> 
  point_plot(balance ~ bank_account_type)

acct_type_balance |> 
  ggplot(aes(bank_account_type, balance)) +
  geom_point()

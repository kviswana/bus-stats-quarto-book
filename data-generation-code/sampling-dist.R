set.seed(123)
sample_means <- customers_population |> 
  slice_sample(n = 50, replace = TRUE) |> 
  summarize(avg_customers = mean(customers)) |> 
  trials(5000)

sample_means |> 
  write_csv("lst-extras-datasets/cust_sample_means.csv")

sample_means |> 
  point_plot(avg_customers ~ 1, annot = "violin", point_ink = 0)

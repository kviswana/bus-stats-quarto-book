num <- c(2, 4, 5, 9)
variance_example <- data.frame(num)
variance_example |> 
  summarize(num_var = var(num))

mpg |> 
  point_plot(cty ~ displ + class)

write_csv(variance_example, "lst-extras-datasets/variance_examples.csv")

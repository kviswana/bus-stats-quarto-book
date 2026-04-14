Boston_marathon |> 
  mutate(model_estimate = model_values(minutes ~ year),
         residual = minutes - model_estimate) |> 
  summarize(Total_variance = var(minutes), 
            Model_estimates_variance = var(model_estimate), 
            Residual_variance = var(residual))

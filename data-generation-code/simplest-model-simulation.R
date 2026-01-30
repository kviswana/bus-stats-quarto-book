## First three plays of the game -- 20 trials each

outcomes <- c(4, 2, 6, 1, 8, 1, 3, 9, 2, 4)
mean(outcomes)
sum(outcomes)

set.seed(3245)
play_commitment_game <- function(decision,
                                 outcomes = c(4, 2, 6, 1, 8, 1, 3, 9, 2, 4),
                                 n_trials = 20,
                                 seed = NULL) {
  
  if (!is.numeric(decision) || length(decision) != 1 || is.na(decision)) {
    stop("decision must be a single numeric value.")
  }
  if (decision <= 0) {
    stop("decision must be positive.")
  }
  
  if (!is.null(seed)) set.seed(seed)
  
  # --- Simulate the game ---
  random_choice <- sample(outcomes, size = n_trials, replace = TRUE)
  diff <- decision - random_choice
  penalty <- diff^2
  
  results <- data.frame(
    random_choice = random_choice,
    diff = diff,
    penalty = penalty
  )
  
  total_penalty <- sum(penalty)
  
  # --- Table 1: Decision ---
  decision_tbl <- data.frame(
    item = "decision =",
    value = decision
  )
  
  # --- Table 3: Total penalty ---
  total_tbl <- data.frame(
    item = "Total penalty =",
    value = total_penalty
  )
  
  # --- Print tables (Markdown) ---
  print(knitr::kable(decision_tbl, col.names = c("", "")))
  cat("\n\n")
  print(knitr::kable(results))
  cat("\n\n")
  print(knitr::kable(total_tbl, col.names = c("", "")))
  
  invisible(list(
    decision = decision,
    results = results,
    total_penalty = total_penalty
  ))
}

play_commitment_game(6)

play_commitment_game(7)

play_commitment_game(4)



## Now we oplot the total penalty for different values of guess with 1000 trials for each value oif guess.

library(ggplot2)

set.seed(123)  # for reproducibility (optional but recommended)

# Given data
nums <- c(4, 2, 6, 1, 8, 1, 3, 9, 2, 4)

# 1. Trial values
trial <- seq(1, 9, by = 0.1)

# 2–3. Compute total penalty for each trial value
results <- data.frame(
  trial = trial,
  total_penalty = sapply(trial, function(t) {
    sampled_nums <- sample(nums, size = 10000, replace = TRUE)
    err_sq <- (sampled_nums - t)^2
    sum(err_sq)
  })
)

# 4. Plot
ggplot(results, aes(x = trial, y = total_penalty)) +
  geom_line() +
  labs(x = "trial", y = "total_penalty") +
  scale_x_continuous(breaks = seq(1, 9, 0.5))

results |> 
  kable()


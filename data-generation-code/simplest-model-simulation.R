
## This file contains to simulate the game of "Simplest model" where the mean is the best guess. IT generates a random array
## Then it chooses a random number from the array as a "choice". It then generates a guess -- if the option is "random" it 
## generates a random guess from among the numbers. If the option is "Optimal" then it uses the mean as the guess. One version 
## reports each trial as a markdown table for inclusion in other documents like Quarto or Obsidian. Another version simply reports the SSEs.

generate_random_numbers <- function() {
  # Generate nums until the average is a whole number
  repeat {
    nums <- sample(10:50, 10, replace = TRUE)
    if (mean(nums) %% 1 == 0) break
  }
  nums
}

## Function to generate markdown tables of each trial
calc_diff_table <- function(nums, guess_type = c("Random", "Optimal")) {
  guess_type <- match.arg(guess_type)
  choices <- integer(20)
  guesses <- numeric(20)
  diffs <- numeric(20)
  
  avg_val <- mean(nums)
  
  for (i in 1:20) {
    choice <- sample(nums, 1)
    guess <- if (guess_type == "Random") sample(nums, 1) else avg_val
    diff_sqr <- (guess - choice)^2
    
    choices[i] <- choice
    guesses[i] <- guess
    diffs[i] <- diff_sqr
  }
  
  # Build markdown table
  lines <- character(0)
  lines <- c(lines, "| number | guess | diff_sqr |")
  lines <- c(lines, "|---:|---:|---:|")
  
  for (i in 1:20) {
    lines <- c(lines, sprintf("| %d | %s | %s |",
                              choices[i],
                              guesses[i],
                              diffs[i]))
  }
  
  lines <- c(lines, sprintf("| **Total** |  | %s |", sum(diffs)))
  
  paste(lines, collapse = "\n")
}

## Function to run experiment and return markdown tables
run_experiment_md <- function(num_rand_trials) {
  nums <- generate_random_numbers()
  mds <- character(num_rand_trials + 1)
  for (i in 1:num_rand_trials) {
    # gsub so that we can print a markdown to copy and paste into Quarto or Obsidian
    mds[i] = gsub("\\\\n", "\n", calc_diff_table(nums, "Random"))
  }
  mds[num_rand_trials + 1] = gsub("\\\\n", "\n", calc_diff_table(nums, "Optimal"))
  mds
  }

## Run the experiment
mds <- run_experiment_md(9) 

## Function to just report the sum of squared errors for one game play no markdown of all the details

calc_diff <- function(nums, guess_type = c("Random", "Optimal")) {
  guess_type <- match.arg(guess_type)
  choices <- integer(20)
  guesses <- numeric(20)
  diffs <- numeric(20)
  
  avg_val <- mean(nums)
  
  for (i in 1:20) {
    choice <- sample(nums, 1)
    guess <- if (guess_type == "Random") sample(nums, 1) else avg_val
    diff_sqr <- (guess - choice)^2
    
    choices[i] <- choice
    guesses[i] <- guess
    diffs[i] <- diff_sqr
  }
  sum(diffs)
}

run_experiment <- function(num_rand_trials) {
  nums <- generate_random_numbers()
  sse <- integer(num_rand_trials + 1)
  for (i in 1:num_rand_trials) {
    sse[i] = calc_diff(nums, "Random")
  }
  sse[num_rand_trials + 1] = calc_diff(nums, "Optimal")
  sse
}

sses <- run_experiment(100)

hist(run_experiment(100))


library(dplyr)
library(ggplot2)


## First find for the kiosk data with kiosk identification
## what is the average penalty if we 
set.seed(123)  # optional, for reproducibility

# 1) overall average
avg_cust <- mean(kiosk_mall_beach$customers)

num_trials = 10000
random_choice <- sample(kiosk_mall_beach$customers, 
                        size = num_trials, replace = TRUE)
diff <- random_choice - avg_cust
mean(diff^2)


## Now try different guesses for the the two kiosks and see if average is still best

library(dplyr)
library(ggplot2)

set.seed(123)  # optional

# 1) Averages by kiosk type
kiosk_avgs <- kiosk_mall_beach %>%
  group_by(kiosk) %>%
  summarise(avg = mean(customers), .groups = "drop")

mall_avg  <- kiosk_avgs %>% dplyr::filter(kiosk == "Mall")  %>% pull(avg)
beach_avg <- kiosk_avgs %>% dplyr::filter(kiosk == "Beach") %>% pull(avg)

mall_avg
beach_avg

# 2) Simulation over guess grid, separately for each kiosk type
guess_grid <- seq(75, 194, by = 0.05)
n_trials <- 10000

results <- kiosk_mall_beach %>%
  group_by(kiosk) %>%
  summarise(
    data = list(customers),
    .groups = "drop"
  ) %>%
  mutate(
    sim = purrr::map(data, function(vals) {
      tibble(guess = guess_grid) %>%
        mutate(
          avg_penalty = purrr::map_dbl(guess, function(g) {
            choices <- sample(vals, size = n_trials, replace = TRUE)
            mean((choices - g)^2)
          })
        )
    })
  ) %>%
  select(kiosk, sim) %>%
  tidyr::unnest(sim)

# 3) Plot: one panel per kiosk (easy to compare)
ggplot(results, aes(x = guess, y = avg_penalty)) +
  geom_line() +
  facet_wrap(~ kiosk, scales = "free_y") +
  labs(x = "guess", y = "avg_penalty") +
  theme_minimal()

# If you truly want two separate plots instead of facets:
# for (k in unique(results$kiosk)) {
#   p <- ggplot(filter(results, kiosk == k), aes(guess, avg_penalty)) +
#     geom_line() +
#     labs(x = "guess", y = "avg_penalty") +
#     ggtitle(k) +
#     theme_minimal()
#   print(p)
# }

}


results |> 
  dplyr::filter(kiosk == "Beach", 
         guess == 146)

results |> 
  dplyr::filter(kiosk == "Mall", 
                guess == 101)

kiosk_mall_beach |> 
  point_plot(customers ~ kiosk, annot = "violin")

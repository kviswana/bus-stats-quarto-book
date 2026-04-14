library(dplyr)
library(readr)

# If you're running this in the same environment where the file is mounted:
infile <- "/mnt/data/kiosk_beach_mall_temp.csv"
# Otherwise, use your local path, e.g.:
infile <- "kiosk_beach_mall_temp.csv"

set.seed(123)  # for reproducibility

small <- read_csv(infile, show_col_types = FALSE) %>%
  select(customers, kiosk)

# Bootstrap (sample with replacement) within each kiosk to retain each kiosk's
# observed distribution of customers, expanding to 500 rows per kiosk.
big <- small %>%
  group_by(kiosk) %>%
  slice_sample(n = 500, replace = TRUE) %>%
  ungroup()

# Sanity check: should be 1000 total, 500 per kiosk
stopifnot(nrow(big) == 1000)
print(count(big, kiosk))

# Optional: write it out
write_csv(big, "kiosk_customers_population.csv")

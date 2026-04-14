# Generate a 40-store dataset with:
# store_id, store_manager_name, store_sales, store_floor_area

set.seed(123)

n_stores <- 40

# 1) store_id
store_id <- sprintf("S%03d", 1:n_stores)

# 1) Unique store_manager_name (first + last)
first_names <- c(
  "Ava","Liam","Mia","Noah","Emma","Ethan","Olivia","Lucas","Sophia","Mason",
  "Isabella","Logan","Amelia","Elijah","Harper","James","Evelyn","Benjamin","Ella","Henry",
  "Grace","Michael","Chloe","Daniel","Aria","Matthew","Layla","Jackson","Nora","Sebastian",
  "Riley","David","Zoey","Joseph","Lily","Samuel","Hannah","Carter","Leah","Owen"
)

last_names <- c(
  "Patel","Kim","Garcia","Nguyen","Johnson","Singh","Chen","Martinez","Brown","Taylor",
  "Williams","Rodriguez","Lee","Hernandez","Lopez","Gonzalez","Wilson","Anderson","Thomas","Moore",
  "Jackson","Martin","Thompson","White","Harris","Clark","Lewis","Robinson","Walker","Perez",
  "Hall","Young","Allen","Sanchez","Wright","King","Scott","Green","Baker","Adams"
)

# Ensure exactly 40 unique full names
store_manager_name <- paste(first_names[1:n_stores], last_names[1:n_stores])

# 2) floor_area ~ Normal(mean=5000, sd=1000), rounded to nearest 100 sqft
#    Also guard against non-positive values just in case.
raw_area <- rnorm(n_stores, mean = 5000, sd = 1000)
store_floor_area <- round(raw_area / 100) * 100
store_floor_area <- pmax(store_floor_area, 500)  # enforce a sensible positive minimum

# 3) sales/sqft ~ Lognormal with ORIGINAL-SCALE mean=500 and sd=100
#    Convert (mean, sd) on original scale -> (meanlog, sdlog).
mean_per_sqft <- 500
sd_per_sqft   <- 100

sdlog  <- sqrt(log(1 + (sd_per_sqft^2 / mean_per_sqft^2)))
meanlog <- log(mean_per_sqft) - 0.5 * sdlog^2

sales_per_sqft <- rlnorm(n_stores, meanlog = meanlog, sdlog = sdlog)

# Store sales approximately proportional to floor area
store_sales <- store_floor_area * sales_per_sqft

# Build final data frame
stores <- data.frame(
  store_id = store_id,
  store_manager_name = store_manager_name,
  store_sales = round(store_sales, 0),       # dollars, rounded to whole
  store_floor_area = store_floor_area,
  stringsAsFactors = FALSE
)

stores

stores |> 
  mutate(sps = store_sales/store_floor_area) |> 
  arrange(-sps)


store_sales = stores

write_csv(store_sales, "store_sales.csv")

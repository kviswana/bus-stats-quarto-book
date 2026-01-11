set.seed(123)

n <- 100

# Age of home (years)
age <- round(runif(n, min = 0, max = 70))

# Floor area (sq ft), loosely increasing with newer homes
floor_area_sqft <- round(
  rnorm(
    n,
    mean = 1800 + (20 - age) * 5,
    sd = 400
  )
)
floor_area_sqft <- pmax(floor_area_sqft, 900)

# Number of bedrooms
number_of_bedrooms <- pmin(
  pmax(round(floor_area_sqft / 500), 2),
  5
)

# Number of bathrooms
no_of_bathrooms <- pmin(
  pmax(
    number_of_bedrooms - rbinom(n, 1, 0.4),
    1
  ),
  4
)

# Price model (East Brunswick–style suburban pricing)
price <- round(
  120000 +
    floor_area_sqft * 320 +
    number_of_bedrooms * 15000 +
    no_of_bathrooms * 25000 -
    age * 1800 +
    rnorm(n, 0, 40000),
  -3
)

homes <- data.frame(
  age,
  floor_area_sqft,
  number_of_bedrooms,
  no_of_bathrooms,
  price
)

# Inspect
summary(homes)

write_csv(homes, "lst-extras-datasets/homes.csv")

homes |> 
  model_train(price ~ floor_area_sqft) |> 
  R2()


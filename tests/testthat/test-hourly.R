# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

# Create sample objects (data frames) for testing the hourly() function
# single location
d1 <- sample_df(locations = 1, days = 10, start_date = "2024-03-01")
d1_no_key <- d1 %>% dplyr::select(-location_key) # remove location_key
# 3 locations
d3 <- sample_df(locations = 3, days = 5,  start_date = "2023-01-01")
d3_no_key <- d3 %>% dplyr::select(-location_key) # remove location_key
# 101 locations
d101 <- sample_df(locations = 101, days = 3,  start_date = "2020-07-19")
d101_no_key <- d101 %>% dplyr::select(-location_key) # remove location_key


# Expect messages about locations_key
test_that("messages work when a location key already exists, 1 location", {
  expect_message(hourly(d1, add_location_key = TRUE),
                 regexp = "Existing location key kept")
})

test_that("messages work when a location key already exists, many locations", {
  expect_message(hourly(d101, add_location_key = TRUE),
                 regexp = "Existing location key kept")
})

test_that("messages work when a new location key is added, 1 location", {
  expect_message(hourly(d1_no_key, add_location_key = TRUE),
                 regexp = "New location key added")
})

test_that("messages work when a location key is added, 3 locations", {
  expect_message(hourly(d3_no_key, add_location_key = TRUE),
                 regexp = "New location key added")
})

# Expect errors if the input dataframe is missing variables or variables
# are the wrong class
test_that("errors work if input dataframe is missing required variables", {
  expect_error(hourly(d1[, c("lon", "min", "max")]), # missing date, lat,
               "Input data frame is missing the following required variables:")
})

test_that("errors work if variables of input dataframe are the wrong class", {
  df_wrong <- d1 %>%
    mutate(min  = as.character(min),
           date = as.character(date))
  expect_error(hourly(df_wrong), "must be class")
})

# test the error outputs one row per variable that has wrong class
# Add.


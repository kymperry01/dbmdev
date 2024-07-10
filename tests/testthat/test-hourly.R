# Create sample objects (data frames) for testing the hourly() function

# single location
d1 <- daily(locations = 1, days = 10, start_date = "2024-03-01")
d1_no_key <- d1 %>% dplyr::select(-location_key) # remove location_key

# 50 locations
d50 <- daily(locations = 50, days = 3,  start_date = "2020-07-19")
d50_no_key <- d50 %>% dplyr::select(-location_key) # remove location_key


# Expect messages about locations_key
test_that("messages work when a location key already exists, 1 location", {
  expect_message(hourly(d1, add_location_key = TRUE),
                 regexp = "Existing location key kept")
})

test_that("messages work when a location key already exists, many locations", {
  expect_message(hourly(d50, add_location_key = TRUE),
                 regexp = "Existing location key kept")
})

test_that("messages work when a new location key is added, 1 location", {
  expect_message(hourly(d1_no_key, add_location_key = TRUE),
                 regexp = "New location key added")
})

test_that("messages work when a location key is added, 3 locations", {
  expect_message(hourly(d50_no_key, add_location_key = TRUE),
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

# Check output for a chronologically complete time series
# with no duplicated datetimes. The same test takes care of both
test_that("time series is complete for a single location", {
  h1 <- hourly(d1)
  complete_series <- seq(min(h1$datetime), max(h1$datetime), by = "hour")
  expect_equal(h1$datetime, complete_series)
})



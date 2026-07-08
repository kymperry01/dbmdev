# Create sample objects (data frames) for testing the hourly() function

# single location
d1 <- daily(locations = 1, days = 10, start_date = "2024-03-01", seed = 42)
d1_no_key <- d1 %>% dplyr::select(-location_key) # remove location_key

# 50 locations
d50 <- daily(locations = 50, days = 3,  start_date = "2020-07-19", seed = 42)
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
test_that("errors work if the input dataframe is empty", {
  expect_error(hourly(d1[0, ]), "The input data frame is empty")
})

test_that("errors work if input dataframe is missing required variables", {
  expect_error(hourly(d1[, c("lon", "min", "max")]), # missing date, lat,
               "Input data frame is missing the following required variables:")
})

test_that("errors work if variables of input dataframe are the wrong class", {
  df_wrong <- d1 %>%
    dplyr::mutate(min  = as.character(min),
                  date = as.character(date))
  expect_error(hourly(df_wrong), "The following columns from df should be the classes listed:")
})

test_that("error output lists each variable with the wrong class", {
  df_wrong <- d1 %>%
    dplyr::mutate(lat = as.character(lat),
                  min = as.character(min),
                  date = as.character(date))

  expect_error(hourly(df_wrong), "Variable named lat must be class numeric")
  expect_error(hourly(df_wrong), "Variable named date must be class Date")
  expect_error(hourly(df_wrong), "Variable named min must be class numeric")
})

test_that("output includes generated location keys when requested", {
  h1 <- hourly(d1_no_key, add_location_key = TRUE)

  expect_true("location_key" %in% names(h1))
  expect_equal(unique(h1$location_key), "loc1")
})

test_that("keep_suntimes preserves the sunlight columns", {
  h1 <- hourly(d1, keep_suntimes = TRUE)

  expect_true(all(c("sunrise", "sunset", "tz") %in% names(h1)))
  expect_false(any(is.na(h1$sunrise)))
  expect_false(any(is.na(h1$sunset)))
  expect_false(any(is.na(h1$tz)))
})

# Check output for a chronologically complete time series
# with no duplicated datetimes. The same test takes care of both
# test_that("time series is complete for a single location", {
#   h1 <- hourly(d1)
#   complete_series <- seq(min(h1$datetime), max(h1$datetime), by = "hour")
#   expect_equal(h1$datetime, complete_series)
# })

test_that("each location has a complete hourly time series with unique datetimes", {
  h50 <- hourly(d50)

  by_location <- split(h50$datetime, h50$location_key)
  checks <- lapply(by_location, function(datetime) {
    expected <- seq(min(datetime), max(datetime), by = "hour")
    identical(datetime, expected)
  })

  expect_true(all(unlist(checks)))
  expect_equal(
    nrow(h50),
    nrow(dplyr::distinct(h50, location_key, datetime))
  )
})



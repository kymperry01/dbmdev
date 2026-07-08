test_that("errors when input dates are in the wrong format", {
  expect_error(
    daily(start_date = "wrong format"), "Failed to parse start_date"
  )
})

test_that("daily returns the expected structure for one location", {
  out <- daily(days = 5, start_date = "2024-03-01", seed = 42)

  expect_s3_class(out, "data.frame")
  expect_named(out, c("location_key", "lat", "lon", "date", "min", "max"))
  expect_equal(nrow(out), 5)
  expect_equal(out$location_key, rep("loc1", 5))
  expect_equal(length(unique(out$lat)), 1)
  expect_equal(length(unique(out$lon)), 1)
  expect_s3_class(out$date, "Date")
  expect_equal(out$date, seq(as.Date("2024-03-01"), by = "day", length.out = 5))
  expect_true(all(out$max > out$min))
})

test_that("daily is reproducible when a seed is supplied", {
  out1 <- daily(locations = 2, days = 4, start_date = "2024-03-01", seed = 99)
  out2 <- daily(locations = 2, days = 4, start_date = "2024-03-01", seed = 99)

  expect_equal(out1, out2)
})

test_that("daily creates the requested number of locations and days", {
  out <- daily(locations = 3, days = 4, start_date = "2024-03-01", seed = 123)

  expect_equal(nrow(out), 12)
  expect_equal(unique(out$location_key), c("loc1", "loc2", "loc3"))
  expect_equal(as.integer(table(out$location_key)), c(4, 4, 4))
  expect_equal(length(unique(paste(out$lat, out$lon))), 3)
})

test_that("daily keeps maximum temperatures at least four degrees above the next minimum", {
  out <- daily(days = 6, start_date = "2024-03-01", seed = 7)

  max_to_next_min <- out$max[-nrow(out)] - out$min[-1]
  expect_true(all(max_to_next_min >= 4))
})



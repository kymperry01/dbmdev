d1 <- daily(days = 200, start_date = "2023-09-01", seed = 42)
h1 <- hourly(d1)

test_that("predict_dev example outputs are stable", {

  # See the life stages
  expect_snapshot(dev_params()) # developmental parameters for diamondback moth
  expect_equal(row.names(dev_params()), c("egg", "instar1_2", "instar3", "instar4", "prepupa", "pupa")) # possible values for "start_stage"

  # Predict forward 1 generation from the egg stage
  expect_snapshot(predict_dev(h1, start_date = "2023-09-05"))

  # Predict forward 2 generations from the mid-point of the "instar3" stage
  expect_snapshot(predict_dev(
    h1,
    start_date = "2023-09-02",
    start_stage = "instar3",
    start_dev = 0.5,
    gens = 2
  ))

  # Predict forward 4 generations from "instar4" and output generation times
  expect_snapshot(predict_dev(
    h1,
    start_date = "2023-10-01",
    start_stage = "instar4",
    gens = 4,
    keep = "generations"
  ))

  # Predict back in time 5 generations from the instar1_2 stage.
  # A warning is thrown if you try to predict beyond the available
  # temperature data.
  expect_snapshot(predict_dev(
    h1,
    start_date  = "2024-03-01",
    start_stage = "instar1_2",
    gens = 5,
    direction = "back",
    keep = "generations"
  ))


})

test_that("predict_dev validates required input columns and classes", {
  expect_error(predict_dev(h1[0, ], start_date = "2023-09-05"),
               "The input data frame is empty")

  expect_error(
    predict_dev(h1[, c("datetime", "obs")], start_date = "2023-09-05"),
    "Input data frame is missing the following required variables: location_key"
  )

  h1_wrong <- h1 %>%
    dplyr::mutate(
      location_key = as.factor(location_key),
      datetime = as.character(datetime),
      obs = as.character(obs)
    )

  expect_error(predict_dev(h1_wrong, start_date = "2023-09-05"),
               "Variable named location_key must be class character")
  expect_error(predict_dev(h1_wrong, start_date = "2023-09-05"),
               "Variable named datetime must be class POSIXct")
  expect_error(predict_dev(h1_wrong, start_date = "2023-09-05"),
               "Variable named obs must be class numeric")
})

test_that("predict_dev rejects invalid configuration inputs", {
  expect_error(
    predict_dev(h1, start_date = "2023-09-05", direction = "sideways"),
    "Direction must be 'forward' or 'back'"
  )

  expect_error(
    predict_dev(h1, start_date = "bad-date"),
    "start_date must be a character string in YYYY-MM-DD format"
  )

  expect_error(
    predict_dev(h1, start_date = "2022-01-01"),
    "The specified start_date cannot be found in the"
  )

  expect_error(
    predict_dev(h1, start_date = "2023-09-05", start_stage = "adult"),
    "start_stage must be one of"
  )

  expect_error(
    predict_dev(h1, start_date = "2023-09-05", start_dev = 2),
    "'start_dev' must be a value between 0 and 1"
  )
})

test_that("predict_dev rejects multi-location and duplicate datetime inputs", {
  d2 <- daily(locations = 2, days = 200, start_date = "2023-09-01", seed = 42)
  h2 <- hourly(d2)

  expect_error(
    predict_dev(h2, start_date = "2023-09-05"),
    "Multiple location_keys detected in 'df'"
  )

  h1_dup <- dplyr::bind_rows(h1, h1[1, ])
  expect_error(
    predict_dev(h1_dup, start_date = "2023-09-05"),
    "There are duplicated datetimes in 'df'"
  )
})

test_that("predict_dev handles keep aliases, defaults, and return types", {
  stages_default <- predict_dev(h1, start_date = "2023-09-05")
  stages_alias <- predict_dev(h1, start_date = "2023-09-05", keep = "s")
  increments <- predict_dev(h1, start_date = "2023-09-05", keep = "increments")
  generations <- predict_dev(h1, start_date = "2023-09-05", keep = "g")
  all_outputs <- predict_dev(h1, start_date = "2023-09-05", keep = "all")

  expect_equal(stages_alias, stages_default)
  expect_true(all(c("gen", "stage", "start_dev", "complete_dev") %in% names(stages_default)))
  expect_true(all(c("datetime", "obs", "gen", "stage", "dev", "total_dev", "total_days") %in% names(increments)))
  expect_true(all(c("gen", "stages", "start_dev", "complete_dev", "total_days", "mean_temp_oC") %in% names(generations)))
  expect_type(all_outputs, "list")
  expect_named(all_outputs, c("increments", "stages", "generations"))
})

test_that("predict_dev normalizes aliases for direction and start_dev defaults", {
  forward_default <- predict_dev(h1, start_date = "2023-09-05", keep = "increments")
  forward_explicit <- predict_dev(h1, start_date = "2023-09-05", start_dev = 0, keep = "increments")
  back_default <- predict_dev(h1, start_date = "2024-03-01", direction = "b", keep = "increments")
  back_explicit <- predict_dev(h1, start_date = "2024-03-01", direction = "back", start_dev = 1, keep = "increments")

  expect_equal(forward_default, forward_explicit)
  expect_equal(back_default, back_explicit)
})

test_that("predict_dev errors on an invalid keep value", {
  expect_error(
    predict_dev(h1, start_date = "2023-09-05", keep = "unknown"),
    "should be one of"
  )
})

d1 <- daily(days = 200, start_date = "2023-09-01", seed = 42)
h1 <- hourly(d1)

test_that("multiplication works", {

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
    keep = "gens"
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
    keep = "gens"
  ))


})

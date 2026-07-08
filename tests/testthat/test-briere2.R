daily1  <- daily(days = 90, start_date = "2024-03-01", seed = 42)
hourly1 <- hourly(daily1)


test_that("briere2 example output is stable", {
    par <- dev_params()
    expect_snapshot(par)

    s <- "egg"
    out1 <- briere2(
        df = hourly1,
        a = par[s, "a"],
        Tmin = par[s, "Tmin"],
        Tmax = par[s, "Tmax"],
        m = par[s, "m"],
        direction = "forward"
    )

    expect_snapshot(out1)
})

test_that("briere2 normalizes direction aliases and rejects invalid input", {
    par <- dev_params()
    s <- "egg"

    out_forward <- briere2(
        df = hourly1,
        a = par[s, "a"],
        Tmin = par[s, "Tmin"],
        Tmax = par[s, "Tmax"],
        m = par[s, "m"],
        direction = "forward"
    )
    out_forward_alias <- briere2(
        df = hourly1,
        a = par[s, "a"],
        Tmin = par[s, "Tmin"],
        Tmax = par[s, "Tmax"],
        m = par[s, "m"],
        direction = "F"
    )
    out_back <- briere2(
        df = hourly1,
        a = par[s, "a"],
        Tmin = par[s, "Tmin"],
        Tmax = par[s, "Tmax"],
        m = par[s, "m"],
        direction = "back"
    )
    out_back_alias <- briere2(
        df = hourly1,
        a = par[s, "a"],
        Tmin = par[s, "Tmin"],
        Tmax = par[s, "Tmax"],
        m = par[s, "m"],
        direction = "b"
    )

    expect_equal(out_forward_alias, out_forward)
    expect_equal(out_back_alias, out_back)
    expect_error(
        briere2(hourly1, a = 0.01, Tmin = 10, Tmax = 30, m = 2, direction = "sideways"),
        "Direction must be forward or back"
    )
})

test_that("briere2 only accumulates development within thresholds", {
    df <- data.frame(
        datetime = as.POSIXct("2024-03-01 00:00:00", tz = "UTC") + c(0, 1, 2, 3) * 3600,
        obs = c(5, 10, 20, 30)
    )

    out <- briere2(df, a = 0.001, Tmin = 10, Tmax = 30, m = 2, direction = "forward")

    expect_s3_class(out, "data.frame")
    expect_equal(out$dev[c(1, 2, 4)], c(0, 0, 0))
    expect_gt(out$dev[3], 0)
    expect_equal(out$total_dev, cumsum(out$dev))
})

test_that("briere2 filters completed development for forward and back directions", {
    datetimes <- as.POSIXct("2024-03-01 00:00:00", tz = "UTC") + c(0, 1, 2, 3) * 3600
    df <- data.frame(
        datetime = datetimes,
        obs = rep(20, 4)
    )

    forward <- briere2(df, a = 0.0048, Tmin = 10, Tmax = 30, m = 1, direction = "forward")
    back <- briere2(df, a = 0.0048, Tmin = 10, Tmax = 30, m = 1, direction = "back")

    expect_equal(nrow(forward), 2)
    expect_equal(forward$datetime, datetimes[1:2])
    expect_equal(forward$dev, c(0.4, 0.4), tolerance = 1e-8)
    expect_equal(forward$total_dev, c(0.4, 0.8), tolerance = 1e-8)

    expect_equal(nrow(back), 2)
    expect_equal(back$datetime, rev(datetimes)[1:2])
    expect_equal(back$dev, c(-0.4, -0.4), tolerance = 1e-8)
    expect_equal(back$total_dev, c(0.6, 0.2), tolerance = 1e-8)
    expect_true(all(diff(back$datetime) < 0))
})

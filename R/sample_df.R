#' Create sample daily maximum and minimum Temperature data
#'
#' @description
#' \code{sample_df} generates sample daily maximum and minimum temperature data for
#' one or more locations for test-driving modelling functions in
#' the dbmdev package.
#' Sample geographic coordinates are added to enable interpolation of hourly
#' temperatures using the \code{hourly} function.
#' @param locations The number of locations to include
#' @param days The number of days
#' @param start_date The starting date in YYYY-MM-DD format (character)
#' @param seed A random number that (if set) enables a reproducible \code{data.frame}
#'
#' @return \code{data.frame}
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(tidyr)
#' library(ggplot2)
#'
#' ## Single location with 10 days of sample data
#' daily1 <- sample_df(days = 10)
#' head(daily1)
#'
#' ## 3 locations with 10 days of sample data from a specified date
#' daily3 <- sample_df(locations = 3, days = 10, start_date = "2023-11-30")
#' head(daily3)
#' tail(daily3)
#'
#' ## Plot the data
#' daily1 %>%
#'   pivot_longer(
#'     cols = min:max,
#'     names_to = "Variable",
#'     values_to = "Temperature (oC)"
#'   ) %>%
#'   ggplot(
#'     aes(x = date, y = `Temperature (oC)`, colour = Variable)
#'     ) +
#'   geom_line() +
#'   geom_point(pch = 21, colour = "black") +
#'   theme_bw() +
#'   ggtitle("Sample daily temperature observations at a single location") +
#'   facet_wrap(~location_key)
#'
#' daily3 %>%
#'   pivot_longer(
#'     cols = min:max,
#'     names_to = "Variable",
#'     values_to = "Temperature (oC)"
#'     ) %>%
#'   ggplot(
#'     aes(x = date, y = `Temperature (oC)`, colour = Variable)
#'     ) +
#'   geom_line() +
#'   geom_point(pch = 21, colour = "black") +
#'   theme_bw() +
#'   theme(aspect.ratio = 1) +
#'   labs(x = NULL) +
#'   ggtitle("Sample daily temperature observations at 3 locations") +
#'   facet_wrap(~location_key)
#'
sample_df <- function(
    locations = 1,
    days = 3,
    start_date = "2024-03-01",
    seed = NULL # For reproducible sample data frames
    ) {

  if (!is.null(seed)){set.seed(seed)}

  # check the start date is in the correct format
  check_date <- suppressWarnings(lubridate::as_date(start_date))
  if(is.na(check_date)){
    stop("start_date must be in YYYY-MM-DD format (character).")
  }

  start <- lubridate::as_date(start_date)
  end   <- lubridate::as_date(start_date) + (days - 1)

  adj <- 4 # value (oC) to adjust max temp up by

  data.frame(
    location_key = rep(paste0("loc", seq(1, locations)), each = days),
    # random locations within Australia
    lat  = round(
      rep(stats::runif(locations, min = -43.0, max = -15.0), each = days), 2
      ),
    lon  = round(
      rep(stats::runif(locations, min = 118.0, max = 153.0), each = days), 2
    ),
    date = rep(seq(start, end, by = "day"), times = locations)
    ) %>%
    mutate(min = stats::runif(length(lat), min = -5, max = 15),
           max = min + (10 * stats::runif(1, min = 1.2, max = 2)),
           # to avoid strange looking temperatures, ensure Tmax (day x) exceeds
           # Tmin next day by at least the value of adj
           diff = max - dplyr::lead(min, 1, default = dplyr::last(max) - adj),
           max = ifelse(diff < adj, max + adj - diff, max)) %>%
    dplyr::select(-diff, max)
  }

# # # plot the sample data
# library(dplyr)
# library(tidyr)
# library(ggplot2)
#
#
#
#
# daily3 <- sample_df(locations = 3, days = 10, start_date = "2023-11-30")
#


# daily3 <- sample_df(days = 8, locations = 3)
# hourly3 <- hourly(daily3)
#
# hourly3 %>%
#   ggplot(aes(x = datetime, y = obs)) +
#   geom_line() +
#   geom_point() +
#   facet_wrap(~location_key) +
#   theme_bw() +
#   theme(aspect.ratio = 1) +
#   labs(x = NULL, y = "Temperature (oC)")

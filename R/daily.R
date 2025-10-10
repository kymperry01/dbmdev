#' Create sample daily maximum and minimum temperature data
#'
#' @description
#' \code{daily} generates sample daily maximum and minimum temperature data for
#' one or more locations for test-driving modelling functions in
#' the \code{dbmdev} package.
#' Sample geographic coordinates are added to enable interpolation of hourly
#' temperatures using the \code{hourly} function.
#'
#' @param locations The number of locations to include
#' @param days The number of days
#' @param start_date The starting date in YYYY-MM-DD format (character)
#' @param ... Passed to [lubridate::as_date()]
#' @param seed A random number that (if set) enables a reproducible \code{data.frame}
#'
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
#' daily1 <- daily(days = 10)
#' head(daily1)
#'
#' ## 3 locations with 10 days of sample data from a specified date
#' daily3 <- daily(locations = 3, days = 10, start_date = "2023-11-30")
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
#'   ggplot(aes(x = date, y = `Temperature (oC)`, colour = Variable)) +
#'   geom_line() +
#'   geom_point(pch = 21, colour = "black") +
#'   theme_bw() +
#'   theme(aspect.ratio = 1,
#'     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
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
#'   theme(aspect.ratio = 1,
#'     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#'   labs(x = NULL) +
#'   ggtitle("Sample daily temperature observations at 3 locations") +
#'   facet_wrap(~location_key)
#'
#' @importFrom dplyr mutate lead
#' @importFrom tidyselect all_of
#' @importFrom lubridate as_date
#' @importFrom stats runif
#' @importFrom methods is
#' @export
daily <- function(
    locations = 1L, days = 3L,
    start_date = "2024-03-01", ...,
    seed = NULL # For reproducible sample data frames
) {

  if (!is.null(seed)) set.seed(seed)

  # check the start date is in the correct format
  if (!is(start_date, "Date"))
    start_date <- tryCatch(as_date(start_date, ...), warning = \(w) w)
  if (is(start_date, "simpleWarning")) stop("Failed to parse start_date")

  adj <- 4 # value (oC) to adjust max temp up by
  df <- data.frame(
    location_key = rep(paste0("loc", seq(1, locations)), each = days),
    # random locations within Australia
    lat  = round(
      rep(runif(locations, min = -43.0, max = -15.0), each = days), 2
    ),
    lon  = round(
      rep(runif(locations, min = 118.0, max = 153.0), each = days), 2
    ),
    date = rep(
      seq(start_date, start_date + days - 1, by = "day"), times = locations
    )
  )
  ## Pulling these out of the mutate avoids R CMD check issues
  df$min <- runif(length(df$lat), min = -5, max = 15)
  df$max <- df$min + (10 * runif(1, min = 1.2, max = 2))
  df <- mutate(
    df,
    # to avoid strange looking temperatures, ensure Tmax (day x) exceeds
    # Tmin next day by at least the value of adj
    diff = max - lead(min, 1, default = dplyr::last(max) - adj),
    max = ifelse(diff < adj, max + adj - diff, max)
  )
  dplyr::select(df, -all_of("diff"))

}

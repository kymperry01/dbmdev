#' @title Calculate sunrise and sunset times and daylength for multiple locations and
#' dates.
#'
#' @description A convenient wrapper for \code{\link{maptools::sunriset()}} that takes
#' a \code{dataframe} with multiple dates and locations and outputs sunrise and sunset time
#' in `POSIXct` format and daylength in numeric hours.
#'
#' @details Maptools::sunriset uses the original algorithm from the NOAA solar calculator.
#' @seealso \code{\link{maptools::sunriset()}}
#' @note Option to output the maptools `day_frac` format, specify timezones and change the CRS
#' data projection can be added if it becomes necessary. Note, CHANGES TO SUMMER TIME STILL NEED
#' TO BE ADDRESSED.
#'
#' @references Roger Bivand and Nicholas Lewin-Koh (2016). maptools: Tools for Reading
#' and Handling Spatial Objects. R package version 0.8-39.
#' http://CRAN.R-project.org/package=maptools
#'
#' NOAA solar calculator: http://www.esrl.noaa.gov/gmd/grad/solcalc/sunrise.html
#'
#' @param df A \code{data.frame} with columns named `date` (`as.Date` or
#' `as.POSIXct`), `latitude` and `longitude` in decimal degrees.
#' @return The original \code{data.frame} with added columns for `sunrise`,
#' `sunset`, and `daylength` in numeric hours.
#'
#' @examples
#' library(ggplot2)
#' library(reshape2)
#' library(dplyr)
#' library(lubridate)
#' # create example df
#' dates <- rep(seq(as.Date("2014-01-01"),
#' as.Date("2014-12-31"), by = "day"), times = 2)
#' len <- length(dates)/2
#' df <- data.frame(location = c(rep("Ceduna", len), rep("Murray Bridge", len)),
#'                  date = dates,
#'                  latitude = c(rep(-32.13, len), rep(-35.7, len)),
#'                  longitude = c(rep(133.7, len), rep(139.23, len)))
#'
#' # calculate sunrise, sunset and daylength
#' df2 <- msunriset(df)
#'
#' # plot daylength
#' ggplot(df2, aes(x = date, y = daylength, colour = location)) + geom_line()
#'
#' # plot sunrise and sunset times
#' # NOTE the effect of changing summer time. Needs to be addressed ....
#' df3 <- filter(df2, location == "Ceduna") %>%
#' melt(id.vars = c("date","location"),
#' measure.vars = c("sunrise", "sunset"),
#' variable.name = "direction",
#' value.name = "suntimes")
#'
#' ggplot(df3, aes(x = date, y = hour(suntimes) + minute(suntimes)/60, colour = direction)) + geom_line()
#'
#' @importFrom maptools sunriset
#' @importFrom sp SpatialPoints CRS
#' @import dplyr
#' @importFrom magrittr %<>% extract2
#' @importFrom tibble as_tibble
#'
#' @export
msunriset <- function(df) {

  coords <- sp::SpatialPoints(
    matrix(c(df$longitude, df$latitude), nrow = nrow(df)),
    proj4string = sp::CRS("+proj=longlat +datum=WGS84"))
  dateSeq <- as.POSIXct(df$date)

  df %>% mutate(
    sunrise = maptools::sunriset(
      coords, dateSeq, direction = "sunrise", POSIXct.out = TRUE
      ) %>%
      extract2("time"),
    sunset = maptools::sunriset(
      coords, dateSeq, direction = "sunset", POSIXct.out = TRUE
      ) %>%
      extract2("time"),
    daylength = as.numeric(sunset - sunrise)) %>%
    tibble::as_tibble()
}




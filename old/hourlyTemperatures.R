#' @title Interpolate hourly temperatures from daily temperature data
#'
#' @description Implements Cesaraccio's model (see refs) for interpolating hourly
#' temperatures from daily maximum and minimum temperatures.
#' @details Uses two sine functions and a square root function to estimate the hourly
#' temperatures for three periods of each day. Sunrise and sunset times required
#' for the model can be calculated using \code{\link{dbmdev::msunriset()}}
#' which conveniently handles \code{data.frames} with multiple dates and locations.
#'
#' @param df A \code{data.frame} containing columns called `station` (a unique character
#' string or numeric location or weather station identifier), `date` in \code{as.Date}
#' or \code{as.POSIXct} format, `min` and `max` (numeric daily temperatures), `sunrise` and `sunset`
#' (datetimes in POSIXct format).
#' @return A dataframe with the variables `station`, `datetime` in as.POSIXct format, and
#' the interpolated hourly `obs`.
#'
#' @references
#' Cesaraccio, Carla, et al. "An improved model for determining degree-day values from
#' daily temperature data." International journal of biometeorology 45.4 (2001): 161-169.
#'
#' @import dplyr
#' @importFrom magrittr %<>%
#' @importFrom parallel mclapply
#' @importFrom lubridate ymd_h hour
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' # create a dataframe with daily max and minimum temperatures for one week
#' # include latitude, longitude here for calculation of required sunrise/set times
#' dates <- seq(as.Date("2016-03-01"), as.Date("2016-03-08"), by = "day")
#' df <- tibble(station = "loc1",
#'                  latitude = -31.141980,
#'                  longitude = 138.395350,
#'                  date = dates,
#'                  min = rnorm(n = length(dates), mean = 12, sd = 1),
#'                  max = min + runif(1, min = 6, max = 10)) %>%
#'               dbmdev::msunriset()
#'
#' # estimate hourly temperatures
#' # note, `obs` for the last date in the sequence will contain a string of NAs
#' # because the model uses minimum temperatures from the next day
#' hourly <- hourlyTemperatures(df)
#' ggplot(hourly %>% filter(!is.na(obs)), aes(x = datetime, y = obs)) +
#'   geom_line()
#'
#' @export
hourlyTemperatures <- function(df) {

  # error checks
  # ... check that there are no duplicated dates in the daily df
  if (anyDuplicated(df$date) > 0) {
    stop("There are duplicated dates in the input dataframe. \n
         Each row should contain temperature observations for a unique date")
  }

  # add vars for model inputs (Cesaraccio 2001)
  df %<>%
    mutate(date = as.Date(date)) %>%
    dplyr::rename(Tn = min, # Tmin current day
                  Tx = max) %>% # Tmax current day
    dplyr::mutate(Tp = dplyr::lead(Tn, n = 1), # Tmin nxt day
                  T0 = Tx - 0.39 * (Tx - Tp), # T at sunset
                  Hn = lubridate::hour(sunrise), # sunrise H
                  H0 = lubridate::hour(sunset),  # sunset H
                  Hp = Hn + 24, # sunrise H nxt day
                  Hx = H0 - 4)  # H Tx is reached

  dupRows <- function(df, n){
    nObs <- nrow(df)
    ind <- rep(1:nObs, each = n)
    df[ind, ]
  }

  dfSplit <- split(df, seq(nrow(df)))
  df2 <- dfSplit %>%
    parallel::mclapply(function(x){
      x %<>% dupRows(n = 24) %>%
        mutate(hour = (Hn[1] + 1):Hp[1]) #  sunrise+1 to sunrise next day

      # subset day into three time periods
      t1 <- dplyr::filter(x, hour - Hn > 0, hour - Hx <= 0)
      t2 <- dplyr::filter(x, hour - Hx > 0, hour - H0 <= 0)
      t3 <- dplyr::filter(x, hour - H0 > 0, hour - Hp <= 0)

      # calculate hourly obs
      t1 %<>% dplyr::mutate(obs = Tn + (Tx - Tn)*sin((hour - Hn)/(Hx - Hn)*(pi/2)))
      # note for t2, authors did not include`=`, but this missed sunset hour
      t2 %<>% dplyr::mutate(obs = T0 + (Tx - T0)*sin((pi/2) + ((hour - Hx)/4) * (pi/2)))
      t3 %<>% dplyr::mutate(obs = T0 + ((Tp - T0) / sqrt(Hp - H0))*sqrt(hour - H0))

      dplyr::bind_rows(t1, t2, t3)

    }) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(station, date)

  # correct time phase: attribute hours > 24 to morning of the next day
  tmpDf1 <- df2 %>%
    dplyr::filter(hour <= 23)

  tmpDf2 <- df2 %>%
    dplyr::filter(hour > 23) %>%
    dplyr::mutate(hour = hour - 24,
                  date = date + 1)

  dplyr::bind_rows(tmpDf1, tmpDf2) %>%
    dplyr::mutate(
      datetime = lubridate::ymd_h(paste(date, hour, sep = "-"))
      ) %>%
    dplyr::select(station, datetime, obs) %>%
    dplyr::arrange(station, datetime)

}

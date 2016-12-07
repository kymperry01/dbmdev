#' @title Predict reverse incremental temperature-based development of diamondback moth
#' based on supplied parameters
#'
#' @description Predicts reverse-in-time temperature-dependent development for an
#' individual DBM lifestage based on supplied incremental temperature observations
#' and DBM development parameters.
#'
#' @details Implements Briere's temperature-dependent insect development model,
#' equation 2 (Briere 1999) reverse-in-time. A separate analagous function, \code{\link{fwdBriere}},
#' implements the model forward-in-time. Outputs DBM development for each time increment
#' as a proportion from `0` (no development) to `1` (completed development) for the
#' lifestage. These functions are the workhorses containing Briere's model; they are designed
#' to be called inside other functions that allow the user to control starting parameters
#' (starting date, lifestage etc). See \code{\link{fwdDev()}}.
#'
#' @return The original \code{data.frame} with the added columns `dev` (proportional
#' development for each increment) and `totalDev` (cumulative porportional development).
#' The returned data.frame will only contain the rows/timepoints where `TotalDev` > 0.
#'
#' @param df A \code{data.frame} containing observed temperatures in a variable called
#' `obs`, and the 'datetimes' in POSIXct format.
#' @param a A constant from the Briere model.
#' @param Tmin The minimum temperature development threshold.
#' @param Tmax The maximum temperature development threshold.
#' @param m The power coefficent from the Briere model.
#' @param timeInt The time interval being used to model the data in minutes.
#' The default value is 60.
#'#'
#' @seealso \code{\link{fwdDev}}.
#'
#' @import dplyr
#' @importFrom magrittr %<>%
#'
#' @references Brazzale, A.R. (2005). hoa: An R package bundle for higher order likelihood inference. Rnews, 5/1 May 2005, 20-27. ISSN 609-3631, URL ftp://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf.
#' @references Briere, Jean-Francois, et al. (1999) A novel rate model of temperature-dependent development for arthropods. Environmental Entomology 28.1: 22-29.
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' # A dataframe with half-hourly temperature obs
#' date <- sort(rep(seq(as.Date("2014-06-01"), (as.Date("2014-09-08") + 100), by = "day"), times = 24))
#' hour <- rep(0:23, time = length(date)/24)
#' datetime <- ymd_hms(paste(date, hour), truncated = 2)
#' obs <- runif(n = length(date), min = 0, max = 38)
#' df <- data.frame(datetime, obs)
#'
#' # Predict reverse development
#' dev <- revBriere(df, a = 0.0003592, Tmin = 1.754, Tmax = 35.08, m = 4.347)
#' head(dev)
#' tail(dev)
#'
#' @export
revBriere <- function(df, a, Tmin, Tmax, m, timeInt = 60, ...){
  df %<>% arrange(desc(datetime)) # ensure inverted df
  obs <- df$obs
  fitObs <- intersect(which(obs >= Tmin), which(obs <= Tmax))
  dev <- rep(0, length(obs))
  dev[fitObs] <- (a * obs[fitObs] * (obs[fitObs] - Tmin) * (Tmax - obs[fitObs]) ^ (1/m))/(24*60/timeInt)
  mutate(df, dev = -dev, totalDev = 1 + cumsum(dev)) %>%
    filter(totalDev > 0)
}

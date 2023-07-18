#' @title Predict temperature-development of diamondback moth
#' based on supplied parameters
#'
#' @description Calculates the proportion of development per time
#' increment for an individual lifestage of diamondback moth (DBM) using
#' Briere model formula II and fitted development parameters.
#'
#' @details A modelling engine function that implements Briere's temperature-
#' dependent development model equation 2 (Briere 1999). Calculates
#' incremental and cumulative development per time interval as a proportion
#' from 0 to 1 for an individual DBM lifestage. Called by
#'\code{\link{predict_development()}} to model multiple life stages.
#'
#' @return A \code{tibble} with the original variables and new columns
#' "dev" and "total_dev" added. Only rows where 0 <= "total_dev" < 1 are kept.
#'
#' @param df A \code{data.frame} with a variable named "datetime" (POSIXct) and
#' hourly temperature observations in a variable named "obs" (double).
#' @param a A constant from Briere's model.
#' @param Tmin The minimum temperature development threshold.
#' @param Tmax The maximum temperature development threshold.
#' @param m The power coefficent from the Briere model.
#' @param direction Either "forward" or "back" in time.
#'
#' @import dplyr
#' @import tidyverse
#' @imoprt magrittr # this is for the dev_params object. Rm this dependency
#'
#' @references Briere, Jean-Francois, et al. (1999) A novel rate model of temperature-dependent development for arthropods. Environmental Entomology 28.1: 22-29.
#'
#' @examples
#' library(dplyr)
#' library(lubridate)
#'
#' # Fitted development parameters for diamondback moth
#' # TO DO: Supply this object with the package
#' dev_params <- data.frame(
#'   egg       = c(0.0003592,   1.754,    35.08,     4.347),
#'   instar1_2 = c(0.00036,    -2.9122,   32.4565,  60.5092),
#'   instar3   = c(0.0009688,   0.7893,   32.04,    14.53),
#'   instar4   = c(0.000674,   -1.22561,  32.04873, 15.27334),
#'   prepupa   = c(0.00181533,  3.963006, 33.04467,  4.317786),
#'   pupa      = c(0.000396921, 2.417172, 32.44556, 11.99131)
#'   ) %>%
#'   magrittr::set_rownames(c("a", "Tmin", "Tmax", "m")) %>%
#'   t()
#'
#' # An example df with hourly temperature observations
#' daily_obs <- example_df(
#'   nlocations = 1, # use only 1 here
#'   ndays = 30,
#'   start_date = "2022-01-01",
#'   seed = 24
#'   )
#' hourly_obs <- daily_to_hourly(daily_obs)
#'
#' # Predict development for a single stage
#' rownames(dev_params)
#'
#' # egg
#' egg <- briere2(df = hourly_df,
#'                a = 0.0003592,
#'                Tmin = 1.754,
#'                Tmax = 35.08,
#'                m = 4.347,
#'                direction = "forward")
#'
#' # grab the params directly for any stage
#' stage <- "instar4"
#'
#' instar4 <- briere2(
#'   df = hourly_df,
#'   a    = dev_params[stage, "a"],
#'   Tmin = dev_params[stage, "Tmin"],
#'   Tmax = dev_params[stage, "Tmax"],
#'   m    = dev_params[stage, "m"],
#'   direction = "forward"
#'   )
#'
#' head(instar4)
#' tail(instar4)
#'
#' @export
briere2 <- function(df, a, Tmin, Tmax, m, direction) {

  direction[grepl("^b|B", direction)] <- "back"
  direction[grepl("^f|F", direction)] <- "forward"

  if (!direction %in% c("forward", "back")) {
    stop("Direction must be forward or back")
  }

  if (direction == "back"){
    df <- df %>% dplyr::arrange(desc(datetime))
  }

  obs <- df$obs
  fit_obs <- dplyr::intersect(which(obs >= Tmin), which(obs <= Tmax))
  dev <- rep(0, length(obs))
  dev[fit_obs] <- (a * obs[fit_obs] * (obs[fit_obs] - Tmin) * (Tmax - obs[fit_obs]) ^ (1/m))/(24)

  if (direction == "back"){
    return(
      df %>%
        dplyr::mutate(dev = -dev, total_dev = 1 + cumsum(dev)) %>%
        dplyr::filter(total_dev > 0)
    )
  }

  df %>%
    dplyr::mutate(dev = dev, total_dev = cumsum(dev)) %>%
    dplyr::filter(total_dev < 1)

}

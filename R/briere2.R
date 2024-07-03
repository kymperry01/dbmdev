#' @title Predict temperature-dependent development using Briere's model II
#'
#' @description Calculate hourly development rate for a given insect species
#' and life stage using hourly temperature observations and insect development
#' parameters.
#'
#' This is an internal modelling function called by (PREDICT
#' DEVELOPMENT) to calculate the point in time that development for the modelled
#' life stage is completed. It is not for package users.
#' Models are simply run with the input data provided without considering date
#' range of interest.
#'
#' @details Implements Briere's temperature-dependent development model equation 2 (Briere 1999) to calculate
#' incremental and cumulative development per time interval as a proportion
#' from 0 to 1 for an individual DBM life stage.
#'
#' @return A \code{tibble} with the original variables and new columns
#' "dev" and "total_dev" added. Only rows where 0 <= "total_dev" < 1 are kept.
#'
#' @param df A \code{data.frame} with a variable named "datetime" (POSIXct) and
#' hourly temperature observations in a variable named "obs" (double).
#' @param a Constant from Briere's model.
#' @param Tmin Minimum temperature development threshold.
#' @param Tmax Maximum temperature development threshold.
#' @param m Power coefficient from the Briere model.
#' @param direction Either "forward" or "back" in time. Predictions are simply
#' performed from the start date (direction = "forward") or end (direction =
#' "back") of the provided data.
#'
#' @import dplyr
#'
#' @references Briere, Jean-Francois, et al. (1999) A novel rate model of temperature-dependent development for arthropods. Environmental Entomology 28.1: 22-29.
#'
#' @examples
#'
#' # Some example daily temperature observations
#' dfd <- example_df(ndays = 90, start_date = "2024-03-01", seed = 24)
#' head(dfd)
#'
#' # Interpolate to hourly temperatures
#' dfh <- daily_to_hourly(dfd)
#' head(dfh)
#'
#' #> TO DO: This gives wierd output with the location_crds columns, where tibble
#' #> doesn't display all digits so the pasted cords look different. Fix.
#'
#' # Get the DBM development parameters for Briere's model
#' par <- dev_params()
#' par
#'
#' # Predict development forward in time for the "egg" stage
#' s <- "egg"
#' out1 <- briere2(df   = dfh,
#'                 a    = par[s, "a"],
#'                 Tmin = par[s, "Tmin"],
#'                 Tmax = par[s, "Tmax"],
#'                 m    = par[s, "m"],
#'                 direction = "forward")
#' head(out1)
#' tail(out1) # see the datetime when stage development is complete
#'
#' # Predict development backwards in time for the "instar3" stage
#' s <- "instar3"
#' out2 <- briere2(df   = dfh,
#'                 a    = par[s, "a"],
#'                 Tmin = par[s, "Tmin"],
#'                 Tmax = par[s, "Tmax"],
#'                 m    = par[s, "m"],
#'                 direction = "back")
#' head(out2)
#' tail(out2) # see the datetime when stage development started
#'
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

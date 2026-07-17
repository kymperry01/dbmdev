#' @title Predict temperature-dependent development using Briere's model II
#'
#' @description Calculate hourly development rate for a given insect species
#' and life stage using hourly temperature observations and insect development
#' parameters.
#'
#' This modelling function is called by \code{\link{predict_dev}} to calculate the
#' point in time that development for the modelled life stage is completed.
#' It always models development \emph{forward} through the supplied data, in the
#' row order provided. Backward-in-time prediction is handled by
#' \code{\link{predict_dev}}, not here.
#'
#' @details Implements Briere's temperature-dependent development model equation 2 (Briere 1999) to calculate
#' incremental and cumulative development per time interval as a proportion
#' from 0 to 1 for an individual DBM life stage.
#'
#' @return A \code{data.frame} with the original variables and new columns
#' "dev" and "total_dev" added. Only rows where "total_dev" < 1 are kept.
#'
#' @param df A \code{data.frame} with a variable named "datetime" (POSIXct) and
#' hourly temperature observations in a variable named "obs" (double). Rows are
#' processed in the order supplied.
#' @param a Constant from Briere's model.
#' @param Tmin Minimum temperature development threshold.
#' @param Tmax Maximum temperature development threshold.
#' @param m Power coefficient from the Briere model.
#' @param stage The developmental stage if using values from [dev_params()].
#' Any values supplied for a, m, Tmin or Tmax will overwrite these
#'
#' @references Briere, Jean-Francois, et al. (1999) A novel rate model of temperature-dependent development for arthropods.
#' Environmental Entomology 28.1: 22-29.
#'
#' @examples
#' # Sample hourly temperature observations
#' daily1  <- daily(days = 90, start_date = "2024-03-01")
#' hourly1 <- hourly(daily1)
#' head(hourly1)
#'
#' # Diamondback moth development parameters for Briere's model II
#' par <- dev_params()
#' par
#'
#' # Predict development for the "egg" stage
#' s <- "egg"
#' out1 <- briere2(df   = hourly1,
#'                 a    = par[s, "a"],
#'                 Tmin = par[s, "Tmin"],
#'                 Tmax = par[s, "Tmax"],
#'                 m    = par[s, "m"])
#' head(out1)
#' tail(out1) # see the datetime when stage development is complete
#'
#' # For predictions forward or back in time from a bio-fix, use predict_dev()
#'
#' ## End
#'
#' @export
briere2 <- function(
    df, a = NULL, Tmin = NULL, Tmax = NULL, m = NULL,
    stage = c("egg", "instar1_2", "instar3", "instar4", "prepupa", "pupa")
) {

  stopifnot(is(df, "data.frame"))
  stopifnot("datetime" %in% colnames(df))
  ## Use defaults if not provided
  stage <- match.arg(stage)
  params <- as.list(dev_params()[stage,])
  a <- c(a, params$a)[[1]]
  m <- c(m, params$m)[[1]]
  Tmin <- c(Tmin, params$Tmin)[[1]]
  Tmax <- c(Tmax, params$Tmax)[[1]]
  stopifnot(is.numeric(c(a, m, Tmin, Tmax)))

  obs <- df$obs
  fit_obs <- intersect(which(obs >= Tmin), which(obs <= Tmax))
  dev <- rep(0, length(obs))
  vals <- a * obs[fit_obs] * (obs[fit_obs] - Tmin) * (Tmax - obs[fit_obs]) ^ (1/m)
  dev[fit_obs] <- vals / 24

  df$dev <- dev
  df$total_dev <- cumsum(df$dev)
  df[df$total_dev < 1, ]

}

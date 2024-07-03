#' @title Predict reverse temperature-based development of diamondback moth
#' over multiple lifestages and generations based on supplied parameters
#'
#' @description Predicts reverse-in-time temperature-dependent development for multiple
#' consecutive DBM lifestages and generations, based on supplied parameters. A separate
#' analagous function, \code{\link{fwdDev}}, predicts reverse-in-time development.
#'
#' @details Implements Briere's temperature-dependent insect development model,
#' equation 2 (Briere 1999) reverse-in-time. Outputs DBM development for each time
#' increment as a proportion from `0` (no development) to `1` (completed development) for
#' individual lifestages consecutively over multiple lifestages and generations.
#'
#' @param tempObsDf A dataframe containing temperature observations at specified time intervals
#' in a variable named `obs`, and POSIXct datetimes in a variable named `datetime`.
#'
#' @param devParamsDf A dataframe containing the four insect development parameters required for
#' Briere's model. The parameters `a`, `Tmin`, `Tmax` and `m` are held as separate variables with
#' the parameters for each lifestage on separate rows, with row names as character strings
#' corresponding to each lifestage (when modelling, the user input for `startStage` must match these).
#' @param startDate The starting date for the model as a character string in "yyyy-mm-dd" format.
#' @param startTime A number between 0 and 23 specifying the hour of the day to start the model.
#' The default value is 12 (midday). The model has not yet been tested using other startTimes.
#' @param timeInt The time interval of increments in minutes. Th default value = 12.
#' The model has not yet been tested with other time intervals.
#' @param startStage The starting lifestage as a character string. Must match one of
#' \code{rownames(devParamsDf)}.
#' @param startDev The starting cumulative development point for `startStage` as a proportion
#' from 0 to 1.
#' @param gens The number of consecutive generations to run the model.
#' @param output A character string specifying whether to output all incremental development predictions
#' ("increments", the default value) or predictions summarised for each stage ("stages").
#' @param ... Other arguments to pass to the function.
#'
#' @note The function has not yet been tested with `startTime` other than 12
#' or `timeInt` other than 60. Recommend not to vary these until tested.
#'
#' @return A list. Each element will be a subset of the original \code{data.frame} with
#' the added columns `dev`, `totalDev`, `stage` and `gen`, and containing only the rows/timepoints
#' for a single generation. To combine output into a single \code{\link{dataframe}}, use \code{\link{dplyr::bind_rows}}.
#'
#' @references Brazzale, A.R. (2005). hoa: An R package bundle for higher order
#' likelihood inference. Rnews, 5/1 May 2005, 20-27. ISSN 609-3631,
#' URL ftp://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf.
#' @references Briere, Jean-Francois, et al. (1999) A novel rate model of
#' temperature-dependent development for arthropods. Environmental Entomology 28.1: 22-29.
#'
#' @import dplyr
#' @importFrom magrittr %<>%
#'
#' @examples
#' library(lubridate)
#'
#' # create example dataframe with hourly temperature obs
#' date <- sort(rep(seq(as.Date("2014-06-01"), (as.Date("2014-09-08") + 100), by = "day"), times = 24))
#' hour <- rep(0:23, time = length(date)/24)
#' datetime <- ymd_hms(paste(date, hour), truncated = 2)
#' obs <- runif(n = length(date), min = 0, max = 38)
#' df <- data.frame(datetime, obs)
#'
#' devParams <- data.frame(st1 = c(0.0003592, 1.754, 35.08, 4.347),
#'                      st2 = c(0.00036, -2.9122, 32.4565, 60.5092),
#'                      st3 = c(0.0003592, 1.754, 35.08, 4.347))
#'  rownames(devParams) <- c("a", "Tmin", "Tmax", "m")
#'  devParams <- t.data.frame(devParams)
#'
#' # Back-predict development over 2 generations
#' rdev <- revDev(tempObsDf = df, devParams, startDate = "2014-09-01", startStage = "st3", startDev = 0.5, gens = 2)
#' rdev
#'
#' # With stages summary output
#' rstages <- revDev(tempObsDf = df, devParams, startDate = "2014-09-01", startStage = "st3", startDev = 0.5, gens = 2, output = "stages")
#'
#'# With generations summary output
#' rgens <- revDev(tempObsDf = df, devParams, startDate = "2014-09-01", startStage = "st3", startDev = 0.5, gens = 2, output = "generations")
#'
#'
#' @export
revDev <- function(tempObsDf, devParamsDf, startDate, startTime = 12, startStage,
                   startDev, gens, output = "increments", timeInt = 60, ...){

  # error checks
  if (!startStage %in% rownames(devParamsDf)) stop("Invalid `startStage` argument. Must match one of row.names(devParamsDf)")
  if (startDev < 0 | startDev > 1) stop("startDev must be a value between 0 and 1")
  if (!as.character(startDate) %in% as.character(lubridate::date(tempObsDf$datetime))) {
    stop("startDate is not found in the climate data file. Several issues can cause this:
         1. Climate data time series does not contain the startDate.
         2. There is a problem with the climate data file or it does not exist.
         3. `startDate` argument must be a character string in `yyyy-mm-dd` format.
         4. Arguments have not been correctly matched in function calls.")
  }

  startdt <- ymd_hms(paste(startDate, startTime, sep = "-"), truncated = 2)
  df <- tempObsDf %>%
    filter(datetime <= startdt) %>%
    arrange(desc(datetime)) # invert df
  allStages <- rev(rownames(devParamsDf)) # reverse stages
  stages <- allStages[which(allStages == startStage):length(allStages)]
  out <- vector("list", length(stages))
  gensOut <- vector("list", gens)
  names(out) <- stages
  fitted <- c()

  # code for gen == 1
  for (s in stages){
    tmpDf <- filter(df, !datetime %in% fitted)
    out[[s]] <- do.call(revBriere,
                        c(list(df = tmpDf), devParamsDf[s, ]))
    if (s == startStage) {
      out[[s]] %<>% filter(totalDev > (1 - startDev)) %>%
        mutate(totalDev = totalDev - (1 - startDev))
    }
    out[[s]] %<>% mutate(stage = s, gen = 1)
    fitted <- c(fitted, out[[s]]$datetime)
  }
  gensOut[[1]] <- bind_rows(out)

  # code for gens >= 2
  if (gens >= 2) {

    # prepare newDf; df filtered to exclude gen 1 output
    newDf <- df %>% filter(!datetime %in% fitted)
    stages <- rev(rownames(devParamsDf)) # ensure reverse stages order
    out <- vector("list", length(stages))
    names(out) <- stages

    # loop over gens excluding gen == 1
    for (g in seq(gens)[-1]) {

      for (s in stages){
        tmpDf <- filter(newDf, !datetime %in% fitted)
        out[[s]] <- do.call(revBriere, c(list(df = tmpDf), devParamsDf[s, ]))
        out[[s]] %<>% mutate(stage = s, gen = g)
        fitted <- c(fitted, out[[s]]$datetime)
      }
      gensOut[[g]] <- bind_rows(out)
    }
  }

  # error check: check output is complete
  checkStages <- unique(gensOut[[gens]]$stage)
  endStage <- gensOut[[gens]] %>% filter(stage == max(checkStages))
  endDev  <- sum(endStage$dev) # end dev should be close to zero
  if (length(checkStages) != length(stages) | endDev > 0.05) {
    stop("Predicted dates exceed date range of supplied climate data")
  }

  # add days counter
  combined <- bind_rows(gensOut) %>% # combine gens
    mutate(d = 1/(24*60/timeInt),
           totDays = cumsum(d)) %>%
    dplyr::select(-d)

  splitGens <- vector("list", gens) # split gens again
  for (g in seq(gens))
    splitGens[[g]] <- filter(combined, gen == g)
  names(splitGens) <- paste0("g", seq(gens))

  # set output format (default = "increments")
  otype <- c("increments", "stages", "generations")
  if (!output %in% otype) {stop("Invalid `output` argument")}
  if (output != "increments"){

    # define a function to summarise stages
    summariseStages <- function(df) {
      df %>% group_by(gen, stage) %>%
        summarise(dev0 = min(datetime),
                  dev1 = max(datetime),
                  totDays = max(totDays) - min(totDays),
                  avTempoC = mean(obs)) %>%
        arrange(desc(dev0))
    }
  }

  if (output == "increments") {

    splitGens # output all increments

  } else {
    if (output == "stages") {

      stages <- lapply(splitGens, summariseStages)
      stages

    } else { # for output == "generations"

      # define another function to summarise a list by generations
      summariseGens <- function(list) {
        bind_rows(list) %>%
          group_by(gen) %>%
          summarise(startStage = stage[1],
                    dev0 = min(dev0),
                    dev1 = max(dev1),
                    totDays = sum(totDays),
                    avTempoC = mean(avTempoC))
      }
      gens <- lapply(splitGens, summariseStages) %>%
        lapply(summariseGens)

      bind_rows(gens)
      }
  }
}



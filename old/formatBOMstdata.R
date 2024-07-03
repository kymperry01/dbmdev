#' @title Format a climate dataframe for analysis
#'
#' @description Takes a \code{data.frame} containing half hourly temperature
#' observations in the format provided by the Australian Bureau of Meterology
#' climate data online service. It formats columns and aggregates data to hourly
#' observations.
#'
#' @param df A \code{data.frame} containing half hourly temperature observations
#' rom weather stations, in the format provided by the Australian Bureau of Meterology
#' climate data online service.
#' @param timeInt The time interval of data observation in minutes. Default value is
#' 60 (Currently, this function will only aggregate data to this time interval).
#'
#' @return A reformatted \code{data.frame} with three columns, `station` (numeric
#' weather station code), `datetime` in POSIXct format, and `obs` (hourly temperature
#' observations).
#' @importFrom lubridate ymd_hms
#' @importFrom magrittr set_names
#' @import dplyr
#'
#' @export
formatBOMstdata <- function(df, useDST = TRUE, timeInt = 60) {

  if (useDST == TRUE) {

    # remove the duplicate datetime variables in BOM files
    dupVars <- lapply(c("_1$", "Local standard time"),
                      grepl, names(df)) %>%
      lapply(which) %>%
      unlist()

  } else {

    dupVars <- lapply(c("YYYY$", "MM$", "DD$", "HH24$", "Local time$"),
                      grepl, names(df)) %>%
      lapply(which) %>%
      unlist()
  }

  df <- df[, -dupVars] %>%
    dplyr::select(
      contains("Station"), contains("Year"),
      contains("MM", ignore.case = FALSE),
      contains("DD", ignore.case = FALSE),
      contains("HH", ignore.case = FALSE),
      contains("MI", ignore.case = FALSE),
      contains("Air Temp", ignore.case = FALSE)) %>%
    set_names(c("Station", "Y", "M", "D", "H", "m", "obs")) %>%
    mutate(Date = as.Date(paste(Y, M, D, sep = "-"))) %>%
    group_by(Date, H) %>% # agregate into hourly obs
    summarise(station = as.integer(unique(Station)),
              obs = as.numeric(obs[1])) %>% # take first obs in each hour
    mutate(datetime = ymd_hms(paste(Date, H, sep = "-"),
                              truncated = 2)) %>%
    ungroup() %>%
    dplyr::select(station, datetime, obs)
  df
}

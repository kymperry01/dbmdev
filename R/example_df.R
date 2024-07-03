#' Create an example dataframe with daily max/min temperature data
#' for testing modelling functions
#'
#' @param nlocations The number of locations to include
#' @param ndays The number of days
#' @param start_date The starting date in YYYY-MM-DD format (character)
#' @param seed A random number that (if set) enables reproducible data.frames
#'
#' @return A \code{data.frame}
#' @export
#'
#' @examples
#' # Single location
#' df1 <- example_df(nlocations = 1, ndays = 90, start_date = "2024-03-01")
#' head(df1)
#'
#' # several locations
#' df3 <- example_df(nlocations = 3, ndays = 90, start_date = "2024-03-01")
#' head(df3)
#' tail(df3)
example_df <- function(nlocations = 1, ndays, start_date, seed = NULL) {

  if (!is.null(seed)){set.seed(seed)} # for reproducible data.frames

  # pad num with zeros to minimum width 2
  nch <- ifelse(nchar(nlocations) < 2, 2, nchar(nlocations))

  tibble(
    location_key = rep(
      paste0("loc", str_pad(seq(1, nlocations),
                            width = nch,
                            side = "left",
                            pad = 0)),
      each = ndays),
    lat  = round(rep(runif(nlocations, min = -43.0, max = -15.0), each = ndays), 2),
    lon  = round(rep(runif(nlocations, min = 118.0, max = 153.0), each = ndays), 2),
    date = rep(
      seq(as_date(start_date), as_date(start_date) + (ndays - 1),
          by = "day"),
      times = nlocations),
    min = runif(length(lat), min = 0, max = 25),
    max = min + (10 * runif(1, min = 1.5, max = 2))
  )
} # TO DO: Ensure daily max one day exceeds (by 5oC) the daily min next day
# otherwise example data looks a bit weird

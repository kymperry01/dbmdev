#' Interpolate hourly temperatures from daily maximum and minimum temperatures
#'
#' @description
#' \code{hourly} interpolates hourly temperature observations from daily maximum
#' and minimum temperatures for a given geographic location using the
#' model of Cesaraccio (2001).
#' Hourly temperatures are used to predict temperature-dependent development
#' rates of insects with the function \code{\link{predict_dev}}.
#'
#' @param df A \code{data.frame} of daily maximum and minimum temperature
#' observations in degrees Celsius and geographic coordinates in decimal
#' degree format.
#' The \code{data.frame} must have, at a minimum, five variables
#' named "date" (Date), "lat" (double), "lon" (double), "min" (double), and
#' "max" (double).
#' All columns in the input \code{data.frame} are preserved in
#' the output.
#'
#' @param add_location_key Optionally generate a variable to the output
#' \code{data.frame} with a unique location key (character string) for each set
#' of unique geographic coordinates, if one does not already exist. This is
#' useful when modelling development at multiple locations using the function
#' \code{predict_dev}.
#'
#' @param keep_suntimes Whether to preserve the sunrise and sunset
#' times used for calculations in the output.
#'
#' @return \code{data.frame}
#' @import suncalc lutz
#'
#' @references Cesaraccio et al. (2001). An improved model for determining
#' degree-day values from daily temperature data. International Journal of
#' Biometeorology 45.4 (2001): 161-169.
#'
#' @export
#'
#' @examples
#' library(dplyr)
#' library(ggplot2)
#'
#' # Single location with 3 days
#' daily1  <- daily()
#' head(daily1)
#'
#' hourly1 <- hourly(daily1)
#' head(hourly1)
#' tail(hourly1)
#'
#' # 3 locations with 5 days data from a specified date
#' daily3  <- daily(locations = 3, days = 5, start_date = "2024-03-01")
#' print(daily3)
#'
#' hourly3 <- hourly(daily3)
#' head(hourly3)
#' tail(hourly3)
#' dim(hourly3)
#'
#' # Plot temperatures
#' hourly3 %>%
#'   ggplot(aes(x = datetime, y = obs)) +
#'   geom_line() +
#'   geom_point() +
#'   facet_wrap(~location_key) +
#'   theme_bw() +
#'   theme(aspect.ratio = 1,
#'   axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
#'   labs(x = NULL, y = "Temperature (oC)") +
#'   ggtitle("Hourly temperature observations for 3 locations")
#'
#' @export
hourly <- function(df, add_location_key = FALSE, keep_suntimes = FALSE) {

  # check for a non-empty data.frame
  if (!nrow(df)) stop("The input data frame is empty")

  # check that required variables exist in input data frame
  reqd_vars  <- c("lat", "lon", "date", "min", "max")
  reqd_class <- c("numeric", "numeric", "Date", "numeric", "numeric")
  names(reqd_class) <- reqd_vars
  miss_vars <- reqd_vars[which(!reqd_vars %in% names(df))]

  if (length(miss_vars) > 0) {
    msg <- paste(
      "Input data frame is missing the following required variables:",
      paste(miss_vars, collapse = ", ")
    )
    stop(msg)
  }

  # check that input data frame has correct variable classes
  chk_class <- vapply(reqd_vars, \(x) is(df[[x]], reqd_class[[x]]), logical(1))
  if (!all(chk_class)) {
    wrong <- paste(reqd_vars, reqd_class, sep = ": ")[!chk_class]
    msg <- paste(
      "The following columns from df should be the classes listed:\n",
      paste(wrong, collapse = "\n")
    )
    stop(msg)
  }

  out <- df
  out$date <- lubridate::as_date(out$date) # Does this need to be checked?
  out$tz <- lutz::tz_lookup_coords(out$lat, out$lon, method = "fast", warn = FALSE)
  out_split <- lapply(
    split(out, out$tz),
    \(df) {
      sun_times <- suncalc::getSunlightTimes(
        data = df, keep = c("sunrise", "sunset"), tz = unique(df$tz)
      )
      df$sunrise <- sun_times$sunrise
      df$sunset <- sun_times$sunset
      df
    }
  )
  out <- do.call("rbind", out_split)
  names(out) <- gsub("^m[ia]", "T", names(out))
  out$Tp <- c(out$Tn[-1], NA)                 # Tmin next day
  out$T0 <- out$Tx - 0.39 * (out$Tx - out$Tp) # T at sunset
  out$Hn <- lubridate::hour(out$sunrise)      # sunrise hour
  out$H0 <- lubridate::hour(out$sunset)       # sunset hour
  out$Hp <- out$Hn + 24                       # sunrise hour next day
  out$Hx <- out$H0 - 4                        # hour Tx is reached
  out$location_crds <- paste(out$lat, out$lon, sep = "_") # For grouping
  out_split <- lapply(split(out, seq_len(nrow(out))), .hourly_obs)
  out <- do.call("rbind", out_split)
  rownames(out) <- seq_along(rownames(out))
  out <- subset(out, !is.na(out$obs))
  o <- order(out$location_crds, out$datetime, out$obs)
  out <- out[o, ]

  if ("location_key" %in% names(df)) {
    ## if a location_key is provided, keep it (using crds)
    key_df <- unique(df[c("location_key", "lat", "lon")])
    key <- key_df$location_key
    names(key) <- paste(key_df$lat, key_df$lon, sep = "_")
    out$location_key <- key[out$location_crds]
    message("Existing location key kept")
  } else {
    # only evaluate if a location key does not exist and add key = TRUE
    if (add_location_key) {
      i <- as.integer(as.factor(out$location_crds))
      out$location_key <- paste0("loc", i)
      message("New location key added")
    }

  }

  ret_cols <- c("location_crds", "datetime", "obs", "location_key")
  if (keep_suntimes) ret_cols <- c(ret_cols, c("sunrise", "sunset", "tz"))

  # remove any duplicated datetimes in output. This occasionally happens when
  # rounding to whole hours in lubridate::hour(sunrise/sunset)
  # dplyr::distinct(out, location_key, datetime, .keep_all = TRUE)
  out <- out[ret_cols]
  dups <- do.call("cbind", lapply(out[c("location_key", "datetime")], duplicated))
  dups <- rowSums(dups) == ncol(dups) # Find rows where all values are duplicates
  out[!dups,]
  ## Does this need sorting by anything?

}


#' function to apply to a single df row with the model params
#' @keywords internal
.hourly_obs <- function(df) {

  hour <- seq(df$Hn[1] + 1, df$Hp[1]) # sunrise + 1 to sunrise next day

  # time 1
  h1 <- hour[hour > df$Hn & hour <= df$Hx]
  obs_t1 <- df$Tn + (df$Tx - df$Tn) * sin((h1 - df$Hn)/(df$Hx - df$Hn) * (pi/2))

  # time 2
  h2 <- hour[hour > df$Hx & hour <= df$H0]
  obs_t2 <- df$T0 + (df$Tx - df$T0) * sin((pi/2) + ((h2 - df$Hx)/4) * (pi/2))

  # time 3
  h3 <- hour[hour > df$H0 & hour <= df$Hp]
  obs_t3 <- df$T0 + ((df$Tp - df$T0) / sqrt(df$Hp - df$H0)) * sqrt(h3 - df$H0)

  ## Now expand all columns to match the output
  out <- data.frame(hour = c(h1, h2, h3), obs  = c(obs_t1, obs_t2, obs_t3))
  out$date <- lubridate::as_date(df$date + floor(out$hour / 24))
  out$hour <- out$hour - 24 * floor(out$hour / 24)
  out$datetime <- lubridate::ymd_h(paste(out$date, out$hour, sep = ":"))
  out$location_crds <- paste(df$lat, df$lon, sep = "_")
  out$sunrise <- df$sunrise
  out$sunset <- df$sunset
  out$tz <- df$tz

  # attribute hours > 24 to the morning of the next day
  ret_cols <- c("location_crds", "datetime", "obs", "sunrise", "sunset", "tz")
  # dplyr::arrange(lat) %>% # for sensible numerically ordered location_keys
  out[ret_cols]
}

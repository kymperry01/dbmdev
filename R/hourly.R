#' Interpolate hourly temperatures from daily maximum and minimum temperatures
#'
#' @description
#' \code{hourly} interpolates hourly temperature observations from daily maximum
#' and minimum temperatures for a given geographic location using the
#' model of Cesaraccio (2001).
#' Hourly temperatures are used to predict temperature-dependent development
#' rates of insects with the function \code{\link{predict_development}}.
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
#' \code{predict_development}.
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
hourly <- function(df, add_location_key = FALSE, keep_suntimes = FALSE) {

  # check for a non-empty data.frame
  if(nrow(df) == 0){
    stop("The input data frame is empty")
  }

  # check that required variables exist in input data frame
  reqd_vars  <- c("lat", "lon", "date", "min", "max")
  reqd_class <- c("numeric", "numeric", "Date", "numeric", "numeric")

  miss_vars <- reqd_vars[which(!reqd_vars %in% names(df))]

  if(length(miss_vars) > 0){
    stop(
      paste(
        "Input data frame is missing the following required variables:",
        paste(miss_vars, collapse = ", ")
        )
      )
  }

  # check that input data frame has correct variable classes
  df_class <- sapply(df, class)
  names(reqd_class) <- reqd_vars

  if(!identical(reqd_class[reqd_vars], df_class[reqd_vars])){

    wrong <- which(!df_class[reqd_vars] == reqd_class[reqd_vars])

    get_correct_class <- function(i){
      msg <- paste("Variable named",
             names(reqd_class[wrong][i]),
            "must be class",
            reqd_class[wrong][i])

      if(i == 1) {
        # add line break to start and end if it's the first element
        paste0(" In input dataframe,\n", msg, "\n")
      } else {
        # no line break to if it's the end
        if(i == length(wrong)) msg
      }

    }
    stop(seq_along(wrong) %>% lapply(get_correct_class))
  }

  # function to apply to a single df row with the model params
  hourly_obs <- function(df) {

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

    data.frame(hour = c(h1, h2, h3),
               obs  = c(obs_t1, obs_t2, obs_t3)) %>%
      dplyr::bind_cols(
        df[, c("date", "lat", "lon", "sunrise", "sunset", "tz")]
      ) %>%
      # attribute hours > 24 to the morning of the next day
      dplyr::mutate(date = ifelse(hour >= 24, date + 1, date),
                    date = lubridate::as_date(date),
                    hour = ifelse(hour >= 24, hour - 24, hour),
                    datetime = lubridate::ymd_h(paste(date, hour, sep = ":")),
                    location_crds = paste(lat, lon, sep = "_")) %>%
      # dplyr::arrange(lat) %>% # for sensible numerically ordered location_keys
      dplyr::select(location_crds, datetime, obs, sunrise, sunset, tz) %>%
      data.frame()
  }

  out <- df %>%
    dplyr::mutate(
      date = lubridate::as_date(date),
      tz = lutz::tz_lookup_coords(
        lat, lon, method = "fast", warn = FALSE
        )
      ) %>%
    dplyr::group_split(tz) %>%
    # output sunrise and sunset times in the local timezone
    lapply(function(df) {

      suncalc::getSunlightTimes(
        data = df,
        keep = c("sunrise", "sunset"),
        tz = unique(df$tz)
      ) %>%
        dplyr::left_join(df, by = c("date", "lat", "lon"))

    }) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(Tn = min,                      # Tmin current day
                  Tx = max) %>%                  # Tmax current day
    dplyr::mutate(Tp = dplyr::lead(Tn, n = 1),   # Tmin next day
                  T0 = Tx - 0.39 * (Tx - Tp),    # T at sunset
                  Hn = lubridate::hour(sunrise), # sunrise hour
                  H0 = lubridate::hour(sunset),  # sunset hour
                  Hp = Hn + 24,                  # sunrise hour next day
                  Hx = H0 - 4,                   # hour Tx is reached
                  # purely for grouping
                  location_crds = paste(lat, lon, sep = "_")
                  )  %>%
    dplyr::group_split(location_crds, 1:nrow(.)) %>%
    lapply(hourly_obs) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(location_crds, datetime, obs) %>%
    dplyr::filter(!is.na(obs)) # remove trailing NAs caused by no sunrise for next day

  # if a location_key is provided, keep it (using crds)
  if ("location_key" %in% names(df)) {

    key <- df[, c("location_key", "lat", "lon")] %>%
      dplyr::mutate(location_crds = paste(lat, lon, sep = "_")) %>%
      dplyr::distinct(location_crds, location_key)

    out <- out %>%
      dplyr::left_join(key, by = "location_crds") %>%
      dplyr::select(location_key, everything()) %>%
      dplyr::arrange(location_key, datetime)
    }

  # only evaluate if a location key does not exist and add key = TRUE
  if (!"location_key" %in% names(df) && add_location_key) {

    new_key <- df[, c("lat", "lon")] %>%
      dplyr::distinct(lat, lon) %>%
      dplyr::arrange(lat) %>%
      dplyr::mutate(location_crds = paste(lat, lon, sep = "_"),
                    idx = 1:nrow(.),
                    location_key = paste0("loc", idx)) %>%
      dplyr::select(lat, lon, location_crds, location_key)

    out <- out %>%
      dplyr::left_join(new_key, by = "location_crds") %>%
      dplyr::select(location_key, everything()) %>%
      dplyr::arrange(location_key, datetime)

    message("New location key added")
  }

  if (keep_suntimes){ # to check results
    return(out)
  }

  # if user tries to add a new key when one already exists
  if ("location_key" %in% names(df) && add_location_key) {

    message("Existing location key kept")
  }

  # remove any duplicated datetimes in output. This occasionally happens when
  # rounding to whole hours in lubridate::hour(sunrise/sunset)
  dplyr::select(out, -sunrise, -sunset, -tz) %>%
    dplyr::distinct(location_key, datetime, .keep_all = TRUE)

  }



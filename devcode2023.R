# New development code - 2023

# ##########
# packages #
# ##########

library(suncalc)
library(lutz)
library(tidyverse)
library(lubridate)

# #############
# To do
# #############

# - currently calculates hourly development
# - has an argument to adjust the hourly increments. But we only have a function
# - to calculate hourlyTemperatures at this point, so are other intervals even worthwhile?
# - perhaps simply/delete the time interval.
# - include the parameterisation script in the package. possibly


# ##################################
# DBM model parameters for briere2 #
# ##################################

dev_params <- data.frame(
  egg       = c(0.0003592,   1.754,    35.08,     4.347),
  instar1_2 = c(0.00036,    -2.9122,   32.4565,  60.5092),
  instar3   = c(0.0009688,   0.7893,   32.04,    14.53),
  instar4   = c(0.000674,   -1.22561,  32.04873, 15.27334),
  prepupa   = c(0.00181533,  3.963006, 33.04467,  4.317786),
  pupa      = c(0.000396921, 2.417172, 32.44556, 11.99131)
  ) %>%
  magrittr::set_rownames(c("a", "Tmin", "Tmax", "m")) %>%
  t()

# ####################################################################
# Interpolate hourly temperature obs from daily max/min observations #
# ####################################################################

daily_to_hourly <- function(df, add_location_key = FALSE, keep_suntimes = FALSE) {

  # function to apply to a single row df with the model params
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
      tibble()
  }

  out <- df %>%
    mutate(date = lubridate::as_date(date),
           tz = lutz::tz_lookup_coords(lat, lon,
                                       method = "fast", warn = FALSE)
    ) %>%
    group_by(tz) %>%
    group_split() %>%
    lapply(function(df) {

      # output sunrise and sunset times in the local timezone
      suncalc::getSunlightTimes(
        data = df,
        keep = c("sunrise", "sunset"),
        tz = unique(df$tz)
      ) %>%
        dplyr::left_join(df, by = c("date", "lat", "lon"))

    }) %>%
    dplyr::bind_rows() %>%
    dplyr::rename(Tn = min,     # Tmin current day
                  Tx = max) %>% # Tmax current day
    dplyr::mutate(Tp = dplyr::lead(Tn, n = 1), # Tmin next day
                  T0 = Tx - 0.39 * (Tx - Tp),  # T at sunset
                  Hn = lubridate::hour(sunrise), # sunrise hour
                  H0 = lubridate::hour(sunset),  # sunset hour
                  Hp = Hn + 24, # sunrise hour next day
                  Hx = H0 - 4)  %>% # hour Tx is reached %>%
    group_by(1:nrow(.)) %>%
    group_split() %>%
    lapply(hourly_obs) %>%
    dplyr::bind_rows() %>%
    dplyr::arrange(location_crds, datetime, obs) %>%
    filter(!is.na(obs)) # remove trailing NAs caused by no sunrise for next day

  # if a location_key is provided, keep it (using crds)
  if ("location_key" %in% names(df)) {

    key <- df[, c("location_key", "lat", "lon")] %>%
      dplyr::mutate(location_crds = paste(lat, lon, sep = "_")) %>%
      dplyr::distinct(location_crds, location_key)

    out <- out %>%
      left_join(key, by = "location_crds") %>%
      dplyr::select(location_key, everything()) %>%
      arrange(location_key, datetime)
  }

  # only evaluate if a location key does not exist and add key = TRUE
  if (!"location_key" %in% names(df) && add_location_key) {

    # pad num with zeros to minimum width 2
    nch <- ifelse(nchar(nrow(df)) < 2, 2, nchar(nrow(df)))

    new_key <- df[, c("lat", "lon")] %>%
      dplyr::distinct(lat, lon) %>%
      dplyr::arrange(lat) %>%
      dplyr::mutate(location_crds = paste(lat, lon, sep = "_"),
                    idx = 1:nrow(.),
                    idx = str_pad(idx, width = nch, side = "left", pad = 0),
                    location_key = paste0("loc", idx)) %>%
      dplyr::select(location_crds, location_key)

    out <- out %>%
      left_join(new_key, by = "location_crds") %>%
      dplyr::select(location_key, everything()) %>%
      arrange(location_key, datetime)

    message("New location key added")
  }


  if (keep_suntimes){ # to check results
    return(out)
  }

  # if user tries to add a new key when one already exists
  if ("location_key" %in% names(df) && add_location_key) {

    message("Existing location key kept")
  }

  dplyr::select(out, -sunrise, -sunset, -tz)
}

# ############################################################
# make function for making example dfs for testing functions #
# ############################################################

example_df <- function(nlocations, ndays, start_date, seed = NULL) {

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

# make an example daily df
daily_df <- example_df(nlocations = 3,
                       ndays = 90,
                       start_date = "2022-01-01",
                       seed = 24)
# interpolate to hourly; takes a few seconds to run
hourly_df <- daily_to_hourly(daily_df)

# check obs
hourly_df %>%
  ggplot(aes(x = obs)) +
  geom_histogram(alpha = 0.5, colour = "black", bins = 30) +
  theme_classic()

# plot
hourly_df %>%
  ggplot(aes(x = datetime,
             y = obs,
             colour = location_key)) +
  geom_line() +
  geom_jitter(pch = 21, size =  0.01) +
  facet_wrap(~location_key, ncol = 1)

# check the daily to hourly function
# - no location key
df_no_location_key <- daily_to_hourly(daily_df %>% dplyr::select(-location_key))
df_no_location_key %>% head() # correct - outputs

# - add a new location key
df_add_key <- daily_to_hourly(daily_df %>% dplyr::select(-location_key),
                              add_location_key = TRUE)
df_add_key %>% head # correct

# - existing key already present
df_keep_current_key <- daily_to_hourly(daily_df, add_location_key = TRUE) #
df_keep_current_key %>% head # correct - keeps key and prints msg

# ###################################
# Briere2 modelling engine function #
# ###################################

briere2 <- function(df, a, Tmin, Tmax, m, direction){

  direction[grepl("^b|B", direction)] <- "back"
  direction[grepl("^f|F", direction)] <- "forward"

  if (!direction %in% c("forward", "back")) {
    stop("Direction must be forward or back")
  }

  if (direction == "back"){
    df <- df %>% arrange(desc(datetime))
  }

  obs <- df$obs
  fit_obs <- intersect(which(obs >= Tmin), which(obs <= Tmax))
  dev <- rep(0, length(obs))
  dev[fit_obs] <- (a * obs[fit_obs] * (obs[fit_obs] - Tmin) * (Tmax - obs[fit_obs]) ^ (1/m))/(24)

  if (direction == "back"){
    return(
      df %>%
        mutate(dev = -dev, total_dev = 1 + cumsum(dev)) %>%
        filter(total_dev > 0)
    )
  }

  df %>%
    dplyr::mutate(dev = dev, total_dev = cumsum(dev)) %>%
    dplyr::filter(total_dev < 1)

}

st <- "egg"
briere2(
  df = hourly_df[, -c(1, 2)],
  a    = dev_params[st, "a"],
  Tmin = dev_params[st, "Tmin"],
  Tmax = dev_params[st, "Tmax"],
  m    = dev_params[st, "m"],
  direction = "forward"
  )

# ##############################
# predict development function #
# ##############################

# for testing only
#
# df = hourly_df %>% filter(location_key == "loc1")
# FUN = briere2
# params = dev_params
# direction = "back"
# start_date = "2022-03-31"
# start_hour = 12
# start_stage = "prepupa"
# start_dev = 0.5
# gens = 6

# takes a single location
predict_development <- function(
    df,
    FUN = briere2,
    params,
    start_date,      # YYYY-MM-DD
    start_hour = 12, # hour in 24hr format
    start_stage,
    start_dev = NULL, # set to 0 if dir = fwd and 1 if direction = back
    gens = 1,
    direction,
    keep = NULL # output everything, increments, stages or gens
    ) {

  # catch typos
  direction[grepl("^b|B", direction)] <- "back"
  direction[grepl("^f|F", direction)] <- "forward"

  if (!direction %in% c("forward", "back")) {
    stop("Direction must be forward or back")
  }

  if (!is.null(keep)) {
    keep[grepl("^i|I", keep)] <- "increments"
    keep[grepl("^s|S", keep)] <- "stages"
    keep[grepl("^g|G", keep)] <- "generations"
    if (!keep %in% c("increments", "stages", "generations"))
      keep <- NULL # if mis-specified, keep all output
    }

  # check there is a single location only
  if ("location_key" %in% names(df) && length(unique(df$location_key)) > 1) { # datetimes must be unique
    stop("Multiple location_keys detected in df. Provide data for a single location.")
  }

  if (anyDuplicated(df$datetime) > 0) { # datetimes must be unique
    stop("There are duplicated datetimes in df")
  }

  if (!start_stage %in% rownames(params)) {
    stop(
      "`start_stage` must be a life stage that exists in the `params` object"
      )
  }

  if (!is.null(start_dev) && (start_dev < 0 | start_dev > 1)) {
    stop("`start_dev` must be a value between 0 and 1")
  }
  if (!"datetime" %in% names(df) || !"obs" %in% names(df)) {
    stop("`df` must contain the variables `datetime` and `obs`")
  }

  if (!as.character(start_date) %in% as.character(lubridate::date(df$datetime))) {
    stop(
      paste("`start_date` is not present in the temperature data.",
      "Ensure the temperature df exists and start_date is YYYY-MM-DD")
      )
  }

  if (is.null(start_dev) && direction == "forward") {
    start_dev <- 0
  }

  if (is.null(start_dev) && direction == "back") {
    start_dev <- 1
  }

  startdt <- lubridate::ymd_hms(
    paste(start_date, start_hour, sep = "-"), truncated = 2
    )

  if (direction == "forward") {
    df <- df %>%
        dplyr::filter(datetime >= startdt) %>%
        dplyr::arrange(datetime)
    all_stages <- rownames(params)
  }

  if (direction == "back") { # invert df and stages
    df <- df %>%
      dplyr::filter(datetime <= startdt) %>%
      dplyr::arrange(desc(datetime))
    all_stages <- rev(rownames(params))
  }

  out_gens <- vector("list", gens)
  fitted <- c()

  # gen 1
  stages <- all_stages[which(all_stages == start_stage):length(all_stages)]
  out <- vector("list", length(stages))
  names(out) <- stages

  for (s in stages) {

    tmp_df <- dplyr::filter(df, !datetime %in% fitted)
    out[[s]] <- do.call(
        FUN, c(list(df = tmp_df),
               direction = direction,
               params[s, ])
        ) %>%
      dplyr::mutate(stage = s, gen = 1)

      if (s == start_stage) {

        if (direction == "forward") {
          out[[s]] <- out[[s]] %>%
            dplyr::filter(total_dev < (1 - start_dev)) %>%
            dplyr::mutate(total_dev = total_dev + start_dev)
          }

        if (direction == "back") {
          out[[s]] <- out[[s]] %>%
            dplyr::filter(total_dev > (1 - start_dev)) %>%
            dplyr::mutate(total_dev = total_dev - (1 - start_dev))
          }
      }

    fitted <- c(fitted, out[[s]]$datetime) %>%
      lubridate::as_datetime() # reverts conversion to numeric in gen 1
  }

  out_gens[[1]] <- bind_rows(out)

  # gens > 1
  if (gens >= 2) {

    out <- vector("list", length(all_stages))
    names(out) <- all_stages

    for (g in seq(gens)[-1]) {

      for (s in all_stages){

        tmp_df <- dplyr::filter(df, !datetime %in% fitted)
        out[[s]] <- do.call(
          FUN, c(list(df = tmp_df),
                 direction = direction,
                 params[s, ])
          ) %>%
          dplyr::mutate(stage = s, gen = g)

        fitted <- c(fitted, out[[s]]$datetime)
      }
      out_gens[[g]] <- bind_rows(out)
    }
  }

  # check that output is complete
  max_gen   <- max(which(sapply(out_gens, nrow) > 0))
  max_stage <- dplyr::last(out_gens[[max_gen]][, "stage"])
  max_dev   <- dplyr::last(out_gens[[max_gen]][, "total_dev"])

  msg1 <- "Output may be incomplete."
  msg2 <- "Predictions exceed time series of temperature data."
  msg3 <- paste0("Development is predicted to generation = ", max_gen,
            ", life stage = ", max_stage,
            ", and total development = ", round(max_dev, 4))

  # If there's a location_key in df, add this in error msg.
  # This is mainly for use with predict_development_locations
  if ("location_key" %in% names(df)){
    msg1 <- paste0(unique(df$location_key), ": ", msg1)
  }

  # truncate to remove any empty dfs to avoid later errors
  out_gens <- out_gens[1:max_gen]

  if (direction == "forward") {

    if (max_gen < gens || !max_stage == dplyr::last(all_stages) ||
        max_dev < 0.75) {

      message(paste(msg1, msg2, msg3))
      }
  }

  if (direction == "back") {

    if (max_gen < gens || !max_stage == dplyr::last(all_stages) ||
        max_dev > 0.25) {

      message(paste(msg1, msg2, msg3))
     }
  }

  count_days <- function(df){
    df %>%
      dplyr::mutate(d = 1 / 24,
                    total_days = cumsum(d), 3) %>%
      dplyr::select(-d)
    }

  out_all <- vector("list", 3)
  names(out_all) <- c("increments", "stages", "generations")

  out_all[[1]] <- out_gens %>%
    dplyr::bind_rows() %>%
    count_days() %>%
    dplyr::select(
      datetime, obs, gen, stage, dev, total_dev, total_days
      ) %>%
    as_tibble()

  summarise_stages <- function(df, direction) {

    res <- df %>%
      count_days %>%
      dplyr::group_by(gen, stage) %>%
      dplyr::summarise(
        start_dev = min(datetime),
        complete_dev = max(datetime),
        total_days = max(total_days) - min(total_days),
        mean_temp_oC = mean(obs), # oC may not be suitable for USA users!
        .groups = "drop"
      )

    if (direction == "back") {

      res %>%
        dplyr::arrange(desc(complete_dev)) %>%
        dplyr::select(gen, stage, complete_dev, dplyr::everything())

    } else {
      res
      }
  }

  out_all[[2]] <- out_gens %>%
    lapply(FUN = summarise_stages, direction = direction) %>%
    dplyr::bind_rows()

  summarise_gens <- function(df, direction) {

    res <- df %>%
      count_days %>%
      dplyr::group_by(gen) %>%
      dplyr::summarise(
        start = dplyr::first(stage),
        end =   dplyr::last(stage),
        start_dev = min(datetime),
        complete_dev = max(datetime),
        total_days = max(total_days) - min(total_days),
        mean_temp_oC = mean(obs, na.rm = TRUE),
        .groups = "drop"
      )

    if (direction == "back") {

      res %>%
        dplyr::mutate(stages = paste(start, end, sep = " to ")) %>%
        dplyr::arrange(desc(complete_dev)) %>%
        dplyr::select(-end, -start) %>%
        dplyr::select(gen, stages, complete_dev, dplyr::everything())

      } else if (direction == "forward") {

        res %>%
          dplyr::mutate(stages = paste(start, end, sep = " to ")) %>%
          dplyr::arrange(complete_dev) %>%
          dplyr::select(-end, -start) %>%
          dplyr::select(gen, stages, start_dev, dplyr::everything())

        }
  }
    out_all[[3]] <- out_gens %>%
      lapply(FUN = summarise_gens, direction = direction) %>%
      dplyr::bind_rows()

    if (is.null(keep)) { return (out_all) }

    if (keep == "increments")  {return(out_all$increments)}
    if (keep == "stages")      {return(out_all$stages)}
    if (keep == "generations") {return(out_all$generations)}

    }

# ##################################
# Test it works in both directions #
# ##################################

daily_obs <- example_df(nlocations = 1, ndays = 100,
                             start_date = "2020-03-01", seed = 501)
hourly_obs <- daily_to_hourly(daily_obs)

range(hourly_obs$datetime)
#> "2020-03-01 06:00:00 UTC" "2020-06-08 12:00:00 UTC"
# FIX TIMEZONE IN OUTPUT PLEASE - SHOULD BE IN LOCAL

dev_back <- predict_development( # take a df for a single location
  df = hourly_obs,
  FUN = briere2,
  params = dev_params,
  direction = "back",
  start_date = "2020-06-08",
  start_stage = "prepupa",
  # start_dev = 1,
  gens = 6,
  keep = "gens"
  )

dev_back

dev_fwd <- predict_development(
  df = hourly_obs,
  FUN = briere2,
  params = dev_params,
  direction = "forward",
  start_date = "2020-03-06",
  start_stage = "instar3",
  start_dev = 0.2,
  gens = 3,
  keep = "incre"
  )

dev_fwd

# ###############################
# functions for plotting output #
# ###############################

# plot dev increments v temperature
dev_fwd$increments %>%
  ggplot(
    aes(x = obs,
        y = dev,
        fill = stage)) +
  geom_point(pch = 21) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        aspect.ratio = 0.8) +
  labs(x = expression("Temperature " ( degree*C)),
       y = "Proportion development per time interval (1/d)",
       fill = NULL) +
  scale_fill_brewer(palette = "Set2")

ggsave("DEV INCREMENT.pdf")

library(ggsci)
library(RColorBrewer)
pal <- ggsci::scale_colour_startrek()
# pal <- ggsci::scale_colour_simpsons()
# pal <- ggsci::scale_colour_jco()
# pal <- ggsci::scale_colour_cosmic()
# pal <- ggsci::scale_colour_igv()

dev_fwd$stages %>%
  ggplot(aes(
    x = 0.5,
    xend = 0.5,
    y = start_dev,
    yend = complete_dev,
    colour = stage
  )) +
  geom_segment(
    # lineend = "butt",
    linejoin = "round",
    linewidth = 3
    ) +
  geom_point(
    pch = 21,
    fill = "black",
    colour = "black",
    size = 1
    ) +
  coord_flip() +
  theme_bw() +
  theme(
    aspect.ratio = 0.2,
    panel.grid = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank()
    ) +
  scale_colour_brewer(palette = "Set2") +
  scale_y_datetime(
    date_breaks = "1 week",
    date_labels = "%d-%b"
    ) +
  labs(x = NULL,
       y = NULL,
       colour = NULL)


ggsave("TEST.pdf")

 # ############################################
 # predict development for multiple locations #
 # ############################################

 # Provide a data frame with the location key (char str) and
 # starting development model parameters for each site
 locations_df <- tibble(
   location_key = c("loc01", "loc02", "loc03"),
   start_date  = c("2021-03-01", "2021-03-07", "2021-03-14"),
   # start_hour  = c(12, 12, 15),
   start_stage = c("egg", "instar3", "pupa"),
   start_dev   = c(0.5, 0.75, 0.2)#,
   #gens = c(2, 2, 2)
   )

daily_obs3 <- example_df(nlocations = 3,
                         ndays = 120,
                         start_date = "2021-03-01",
                         seed = 675)

hourly_obs3 <- daily_to_hourly(daily_obs3)

hourly_obs3 %>% head

# testing only
location_df <- locations_df
obs_df <- hourly_obs3
direction = "forward"
FUN = briere2
params = dev_params
gens = 3
keep = "stages"

predict_development_locations <- function(location_df, obs_df, FUN = briere2,
                                          params, direction, gens,
                                          keep = "stages") {

  # check all starting params are supplied
  pars <- c("location_key", "start_date", "start_stage", "start_dev")
  miss_pars <- pars[which(!pars %in% names(location_df))]

  if (length(miss_pars) > 0) {
    stop(
      paste(
      c("The following parameters are missing from location_df: ",
        paste(miss, collapse = ", ")),
      collapse = " "
      )
      )
  }

  # if start_hour is not specified, use default
  if (!"start_hour" %in% names(location_df)) {
    location_df <- dplyr::mutate(location_df, start_hour = 12)
  }

  # check for matching location_keys in location_df and obs_df
  keys <- unique(location_df$location_key)
  miss_keys <- keys[which(!keys %in% unique(obs_df$location_key))]

  if (length(miss_keys) > 0) {
    message(
      paste(
        c("The following locations found in location_df have no matching temperature data in obs_df: ",
          paste(miss_keys, collapse = ", ")),
        collapse = " "
    )
    )
  }

  obs_nest <- obs_df %>%
    tidyr::nest(data = everything(), .by = location_key) %>% # keeps loc_key
    dplyr::rename(obs = data)

  df_nest <- location_df %>%
    tidyr::nest(data = everything(), .by = location_key) %>%
    dplyr::rename(start_params = data) %>%
    left_join(obs_nest, by = "location_key")

  # check start_date appears in all weather dfs
  check_dates <- function(x, y) {
    if(!x$start_date %in% as_date(y$datetime)) {
      x$location_key
    }
  }

  # catch start_date errors before predict_development does
  error_check <- map2(df_nest$start_params, df_nest$obs, check_dates)
  errors <- err_check[which(!sapply(error_check, is.null))]
  if (length(errors) > 0) {
    stop(
      c(
        paste("The specified start_date (location_df) does not exist in",
              "the temperature data time series (obs_df)",
              "for the following locations: "),
        paste(unlist(errors), collapse = ", ")
        )
    )
    }

  run_model <- function(x, y) {
    predict_development(
      df = y,
      FUN = FUN,
      params = params,
      start_date  = x$start_date,
      start_stage = x$start_stage,
      start_hour  = x$start_hour,
      start_dev   = x$start_dev,
      direction = direction,
      gens = gens,
      keep = keep
    )
  }

  df_nest %>%
    mutate(model_fit = map2(start_params, obs, run_model)) %>%
    dplyr::select(location_key, model_fit) %>%
    unnest(cols = c(location_key, model_fit))

}



res <- predict_development_locations(
  locations_df,
  hourly_obs5,
  params = dev_params,
  direction = "forward",
  gens = 3
  )

res
res %>% View



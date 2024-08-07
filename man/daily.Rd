% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/daily.R
\name{daily}
\alias{daily}
\title{Create sample daily maximum and minimum temperature data}
\usage{
daily(locations = 1, days = 3, start_date = "2024-03-01", seed = NULL)
}
\arguments{
\item{locations}{The number of locations to include}

\item{days}{The number of days}

\item{start_date}{The starting date in YYYY-MM-DD format (character)}

\item{seed}{A random number that (if set) enables a reproducible \code{data.frame}}
}
\value{
\code{data.frame}
}
\description{
\code{daily} generates sample daily maximum and minimum temperature data for
one or more locations for test-driving modelling functions in
the \code{\link{dbmdev}} package.
Sample geographic coordinates are added to enable interpolation of hourly
temperatures using the \code{hourly} function.
}
\examples{
library(dplyr)
library(tidyr)
library(ggplot2)

## Single location with 10 days of sample data
daily1 <- daily(days = 10)
head(daily1)

## 3 locations with 10 days of sample data from a specified date
daily3 <- daily(locations = 3, days = 10, start_date = "2023-11-30")
head(daily3)
tail(daily3)

## Plot the data
daily1 \%>\%
  pivot_longer(
    cols = min:max,
    names_to = "Variable",
    values_to = "Temperature (oC)"
  ) \%>\%
  ggplot(aes(x = date, y = `Temperature (oC)`, colour = Variable)) +
  geom_line() +
  geom_point(pch = 21, colour = "black") +
  theme_bw() +
  theme(aspect.ratio = 1,
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  ggtitle("Sample daily temperature observations at a single location") +
  facet_wrap(~location_key)

daily3 \%>\%
  pivot_longer(
    cols = min:max,
    names_to = "Variable",
    values_to = "Temperature (oC)"
    ) \%>\%
  ggplot(
    aes(x = date, y = `Temperature (oC)`, colour = Variable)
    ) +
  geom_line() +
  geom_point(pch = 21, colour = "black") +
  theme_bw() +
  theme(aspect.ratio = 1,
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
  labs(x = NULL) +
  ggtitle("Sample daily temperature observations at 3 locations") +
  facet_wrap(~location_key)

}

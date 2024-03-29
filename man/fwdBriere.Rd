% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fwdBriere.R
\name{fwdBriere}
\alias{fwdBriere}
\title{Predict forward incremental temperature-based development of diamondback moth
based on supplied parameters}
\usage{
fwdBriere(df, a, Tmin, Tmax, m, timeInt = 60)
}
\arguments{
\item{df}{A \code{data.frame} containing observed temperatures in a variable called
\code{obs}, and the 'datetimes' in POSIXct format.}

\item{a}{A constant from the Briere model.}

\item{Tmin}{The minimum temperature development threshold.}

\item{Tmax}{The maximum temperature development threshold.}

\item{m}{The power coefficent from the Briere model.}

\item{timeInt}{The time interval being used to model the data in minutes.
The default value is 60.}
}
\value{
The original \code{data.frame} with the added columns \code{dev} (proportional
development for each increment) and \code{totalDev} (cumulative porportional development).
The returned data.frame will only contain the rows/timepoints where \code{TotalDev} < 1.
}
\description{
Predicts forward-in-time temperature-dependent development for an
individual DBM lifestage based on supplied                                                incremental temperature observations
and DBM development parameters.
}
\details{
Implements Briere's temperature-dependent insect development model,
equation 2 (Briere 1999) forward-in-time. A separate analagous function, \code{\link{revBriere}},
implements the model reverse-in-time. Outputs DBM development for each time increment
as a proportion from \code{0} (no development) to \code{1} (completed development) for the
lifestage. These functions are the workhorses containing Briere's model; they are designed
to be called inside other functions that allow the user to control starting parameters
(starting date, lifestage etc). See \code{\link{fwdDev()}}.
}
\examples{
library(dplyr)
library(lubridate)

# An example dataframe with hourly temperature observations
date <- sort(rep(seq(as.Date("2014-06-01"), (as.Date("2014-09-08") + 100), by = "day"), times = 24))
hour <- rep(0:23, time = length(date)/24)
datetime <- ymd_hms(paste(date, hour), truncated = 2)
obs <- runif(n = length(date), min = 0, max = 38)
df <- data.frame(datetime, obs)

# Predict development
dev <- fwdBriere(df, a = 0.0003592, Tmin = 1.754, Tmax = 35.08, m = 4.347)
head(dev)
tail(dev)

}
\references{
Brazzale, A.R. (2005). hoa: An R package bundle for higher order likelihood inference. Rnews, 5/1 May 2005, 20-27. ISSN 609-3631, URL ftp://cran.r-project.org/doc/Rnews/Rnews_2005-1.pdf.

Briere, Jean-Francois, et al. (1999) A novel rate model of temperature-dependent development for arthropods. Environmental Entomology 28.1: 22-29.
}
\seealso{
\code{\link{fwdDev}}.
}

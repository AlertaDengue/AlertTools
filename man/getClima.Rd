% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_timeseries.R
\name{getClima}
\alias{getClima}
\title{Get Climate Data}
\usage{
getClima(
  cities,
  vars = c("temp_min", "temp_max", "temp_med", "umid_min", "umid_med", "umid_max",
    "precip_tot", "precip_max"),
  finalday = Sys.Date(),
  iniSE = 201501,
  lastSE,
  datasource = con
)
}
\arguments{
\item{cities}{list of geocodes.}

\item{vars}{vector with meteorological variables. Minimum set of vars 
"temp_min", "temp_max". Default: all of them}

\item{finalday}{alternative to lastSE. Default is the last day available. Format = Y-m-d.}

\item{iniSE}{first epiweek. Default is 012015}

\item{lastSE}{last epiweek. Optional. Default is the last available}

\item{datasource}{a valid database connection}
}
\value{
data.frame with the weekly data (cidade data temp_min tmed tmax umin umed umax pressaomin pressaomed pressaomax)
}
\description{
Create weekly climate time series from satellite data in server 
taking the mean of the daily values. Data source: ERA5.
}
\examples{
NOT USE: con <- dbConnect(RSQLite::SQLite(), "../../AlertaDengueAnalise/mydengue.sqlite")
res = getClima(cities = c(3304557), vars=c("temp_min","temp_max") , iniSE = 201801)
res = getClima(cities = c(3304557, 3200300), iniSE = 202003)
tail(res)
}

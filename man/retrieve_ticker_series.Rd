% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fx_v01.R
\name{retrieve_ticker_series}
\alias{retrieve_ticker_series}
\title{Fetch Ticker Price Data Series}
\usage{
retrieve_ticker_series(
  ticker = "AAPL",
  range = "1mo",
  interval = "1d",
  start_date = NULL,
  stop_date = NULL
)
}
\arguments{
\item{ticker}{string, name of the stock of interest. The string '000' returns 
a series with 0 returns (equivalent to holding cash).}

\item{range}{string, time range of interest (e.g., '1mo')}

\item{interval}{string, interval across measurements (e.g., '1d')}

\item{start_date}{date, first day of a range of interest (for custom date ranges). Can be NULL.}

\item{stop_date}{date, last day in the range of interest. Can be NULL.}
}
\value{
list including json parsed data.
}
\description{
Retrieve data for a ticker of interest from Yahoo Finance and compute returns.
}
\details{
Dates and/or times are considered to be in the America/New_York time zone.
}
\examples{
retrieve_ticker_series('COTY', range = '5d')


}
\author{
Damiano Fantini <damiano.fantini@gmail.com>
}

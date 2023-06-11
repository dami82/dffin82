#' Fix URL string
#' 
#' Fix the URL string
#' 
#' @param url string of a URL to fix
#' 
#' @return string with the corrected URL
#' 
#' @author Damiano Fantini <damiano.fantini@@gmail.com>
#' 
#' @examples 
#' fix_url('http://www.google.com')
#'
#' @export
fix_url <- function(url) {
  url <- sub('/*$', '', url)
  url <- paste0(url, '/')
  return(url)
}


#' Build complex URL
#' 
#' Build URL to query Yahoo! Finance
#' 
#' @param base_url string of the base URL
#' @param ticker string corresponding to a ticker of interest
#' @param url_params list of additional parameters
#' 
#' @return string with the URL for querying yahoo finance
#' 
#' @author Damiano Fantini <damiano.fantini@@gmail.com>
#' 
#' @examples 
#' build_url(base_url='http://www.google.com', ticker='AAPL')
#'
#' @export
build_url <- function(base_url, ticker, url_params = NULL) {
  
  # Accepted params
  accepted_pars <- c('period1', 'period2', 'range', 'interval')
  
  y0 <- paste0(base_url, ticker)
  
  y1 <- list()
  if (!is.null(url_params)) {
    if (is.list(url_params)) {
      if (!is.null(names(url_params))) {
        url_params <- url_params[names(url_params) %in% accepted_pars]
        
        for (i in 1:length(url_params)){
          y1[[length(y1) + 1]] <- 
            paste0(names(url_params)[i], '=', url_params[[i]])  
        }
        y1 <- paste(do.call(c, y1), collapse = '&')
      }
    }
  }
  if (is.list(y1) && length(y1) == 0) {
    y1 <- ''
  }
  
  # Complete
  Y <- paste0(fix_url(base_url), 
              ticker)
  if (y1 != '') {
    Y <- paste0(Y, '?', y1)
  }
  return(Y)
}


#' Shift Vector
#' 
#' Shift Values of a Vector
#' 
#' @param x input data, typically a numeric or character vector 
#' @param step integer, number of positions 
#' 
#' @return vector including the shifted values
#' 
#' @author Damiano Fantini <damiano.fantini@@gmail.com>
#' 
#' 
#' @examples 
#' vector_shift(1:5)
#'
#' @export
vector_shift <- function(x, step = 1) {
  
  all_i <- seq(1, length(x), by = 1)
  all_j <- all_i - step
  all_j <- ifelse(all_j %in% all_i, all_j, NA)
  y <- x[all_j]
  return(y)
}


# Convert to Dates 
date_conversion <- function(x) {
  
  y <- tryCatch({
    as.Date(as.POSIXct(as.numeric(x),
                       origin = "1970-01-01", tz = "America/New_York"))
  }, error = function(e) { NULL })
  
  return(y)
}


#' Fetch Ticker Price Data Series
#' 
#' Retrieve data for a ticker of interest from Yahoo Finance and compute returns.
#' 
#' @param ticker string, name of the stock of interest. The string '000' returns 
#' a series with 0 returns (equivalent to holding cash).
#' @param range string, time range of interest (e.g., '1mo')
#' @param interval string, interval across measurements (e.g., '1d')
#' @param start_date date, first day of a range of interest (for custom date ranges). Can be NULL.
#' @param stop_date date, last day in the range of interest. Can be NULL.
#' 
#' @return list including json parsed data. 
#' 
#' @details 
#' Dates and/or times are considered to be in the America/New_York time zone.
#' 
#' @author Damiano Fantini <damiano.fantini@@gmail.com>
#' 
#' @importFrom httr GET content
#' @importFrom jsonlite parse_json
#' 
#' @examples 
#' retrieve_ticker_series('COTY', range = '5d')
#' 
#'
#' @export
retrieve_ticker_series <- function(ticker = 'AAPL', range = '1mo', 
                                   interval = '1d', start_date = NULL,
                                   stop_date = NULL) {
  
  # Base URLs  
  yf_base_url <- 'https://query2.finance.yahoo.com/'
  yf_ticker_path <- 'v8/finance/chart/'
  
  valid_periods <- c("1d", "5d", "1mo", "3mo", "6mo", 
                     "1y", "2y", "5y", "10y", "ytd", "max")
  valid_intervals <- c("1m", "2m", "5m", "15m", "30m", "60m", 
                       "90m", "1h", "1d", "5d", "1wk", "1mo", "3mo")
  
  q_range <- match.arg(arg = range, choices = valid_periods, 
                       several.ok = FALSE)
  q_interval <- match.arg(arg = interval,
                          choices = valid_intervals, several.ok = FALSE)
  
  # Download start date string (YYYY-MM-DD) or _datetime.
  period1 <- NULL; period2 <- NULL
  if (!is.null(start_date) & !is.null(stop_date)) {
    period1 <- tryCatch({
      as.numeric(as.POSIXct(start_date, tz = "America/New_York"))
    }, error = function(e) NULL)
    period2 <- tryCatch({
      as.numeric(as.POSIXct(stop_date, tz = "America/New_York"))
    }, error = function(e) NULL)
  }
  
  # Compose URLs
  base_url <- paste0(yf_base_url, yf_ticker_path)
  
  # Build URL
  if (!is.null(period1) & !is.null(period2)) {
    url_params <- list(period1 = period1, 
                       period2 = period2, interval = q_interval)
  } else {
    url_params <- list(range = q_range, interval = q_interval)
  }
  
  y1_url <- build_url(base_url = base_url, 
                      ticker = ticker, 
                      url_params = url_params)
  
  # Query yfinance
  x1 <- NULL
  x2 <- NULL
  tryCatch({
    x0 <- httr::GET(y1_url)
    x1 <- httr::content(x0, as = 'text', encoding = 'UTF-8')
    x2 <- jsonlite::parse_json(x1)
    y <- parse_price_series(x2)
  }, error = function(e) { NULL })
  
  # output as list
  return(y)
}







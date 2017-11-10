#' Function to decompose a monthly time series with loess. 
#' 
#' @param df Data frame containing a monthly time series. \code{df} must contain
#' \code{"date"} and \code{"value"} variables. 
#' 
#' @author Stuart K. Grange
#' 
#' @seealso \code{\link{stl}}
#' 
#' @return Numeric vector with the length of \code{nrow(df)}.
#' 
#' @export
deseasonalise_stl <- function(df) {
  
  if (!"date" %in% names(df)) 
    stop("`df` must include a `date` variable...", call. = FALSE)
  
  if (!"value" %in% names(df)) 
    stop("`df` must include a `value` variable...", call. = FALSE)
  
  # Test and if not monthly, make monthly
  if (threadr::detect_date_interval(df$date, text_return = TRUE) != "month")
    stop("Time series must have an interval of a month...", call. = FALSE)
  
  # No missing data allowed in loess function
  if (anyNA(df$value)) 
    df$value <- approx(df$value, n = length(df$value))$y
  
  # Create time series object
  ts <- ts(df$value, start = min(df$date), frequency = 12)
  
  # Decompose with loess, use openair defaults
  x <- stl(ts, s.window = 35, robust = TRUE, s.degree = 0)
  # to-do: try stlplus, https://github.com/hafen/stlplus
  
  # order: seasonal, trend, remainder
  # Calculate vector
  x <-  as.vector(x$time.series[, 2]) + as.vector(x$time.series[, 3])
  
  return(x)
  
}


# deseasonalise_kz <- function(df) {
#   
#   if (!"date" %in% names(df)) 
#     stop("`df` must include a `date` variable...", call. = FALSE)
#   
#   if (!"value" %in% names(df)) 
#     stop("`df` must include a `value` variable...", call. = FALSE)
#   
#   # Test and if not daily, make daily
#   if (threadr::detect_date_interval(df$date) != 86400)
#     df <- threadr::aggregate_by_date(df, interval = "day")
#   
#   # Create time series object
#   ts <- ts(df$value)
#   
#   # Apply kz filters, from Wise2005
#   baseline <- kza::kz(ts, m = 15, k = 5)
#   trend <- kza::kz(ts, m = 365, k = 3)
#   
#   # 
#   x <- df$value - baseline
#   x <- x + trend
#   
#   return(x)
#   
# }

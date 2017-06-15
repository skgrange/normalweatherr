#' Function to detect breakpoints in a data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame to detect breakpoints in. 
#' 
#' @param h Minimal segment size either given as fraction relative to the sample 
#' size or as an integer giving the minimal number of observations in each 
#' segment.
#' 
#' @param n Number of breaks to detect. Default is maximum number allowed by
#' \code{h}. 
#' 
#' @return Data frame.
#' 
#' @export
detect_breakpoints <- function(df, h = 0.15, n = NULL) {
  
  # Check
  if (!any(grepl("date", names(df)))) 
    stop("Data must contain a `date` variable.", call. = FALSE)
  
  if (!any(grepl("POSIXct", class(df$date))))
    stop("`date` variable needs to be a parsed date (POSIXct).", call. = FALSE)
  
  # Switch a common variable name
  names(df) <- ifelse(names(df) == "value_predict", "value", names(df))
  
  # Do
  breakpoints <- strucchange::breakpoints(
    value ~ date, 
    data = df, 
    h = h, 
    breaks = n
  )
  
  # Get the dates
  df <- df[breakpoints$breakpoints, c("date")]
  
  # Return data frame
  return(data.frame(df))
  
}

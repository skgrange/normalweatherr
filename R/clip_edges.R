#' Function to ``clip'' the edges of a time-series. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame. 
#' 
#' @param seconds Number of seconds to clip from start and end of time-series. 
#' The default is half a year. 
#' 
#' @param filter Should the observations to be clipped be filtered from \code{df}
#' or invalidated with \code{NA}?
#' 
#' @return Data frame. 
#'
#' @export
clip_edges <- function(df, seconds = 31536000 / 2, filter = FALSE) {
  
  # A catch for dplyr's table
  df <- data.frame(df)
  
  # Get max and min
  date_start <- min(df$date)
  date_end <- max(df$date)
  
  date_start_plus <- date_start + seconds
  date_end_minus <- date_end - seconds
  
  if (filter) {
    
    # Filter
    df <- df[df[, "date"] >= date_start_plus & df[, "date"] <= date_end_minus, ]
    
  } else {
    
    # Invalidate
    df[, "value_predict"] <- ifelse(
      df[, "date"] <= date_start_plus | df[, "date"] >= date_end_minus,
      NA,
      df[, "value_predict"]
    )
    
  }
  
  return(df)
  
}

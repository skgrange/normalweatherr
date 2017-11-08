#' Function to test a time series with the Theil-Sen estimator. 
#' 
#' @param df Data frame. 
#' 
#' @param variable Variable name to test. 
#' 
#' @param deseason Should the time series be deseaonsalised before the trend 
#' test is conducted?
#' 
#' @param auto_correlation Should auto correlation be considered in the estimates?
#' 
#' @seealso \code{\link{TheilSen}}
#' 
#' @return Data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @export
theil_sen_trend_test <- function(df, variable = "value", deseason = FALSE, 
                                 auto_correlation = FALSE) {
  
  # Send plot to dev/null
  pdf(tempfile())
  
  # Do the test without any messages
  quiet(
    suppressMessages(
      df_test <- openair::TheilSen(
        df, 
        pollutant = variable,
        deseason = deseason,
        autocor = auto_correlation
      )$data$res2
    )
  )
  
  dev.off()
  
  # Clean names of returned data frame
  names(df_test) <- stringr::str_replace_all(names(df_test), "\\.", "_")
  
  # Remove duplicate observations
  df_test <- filter(df_test, is.finite(conc))
  
  # Add a variable
  df_test$direction <- ifelse(df_test$slope <= 0, "decreasing", "increasing")
  df_test$direction <- ifelse(df_test$slope == 0, "no_direction", df_test$direction)
  
  # Add more variables
  df_test <- mutate(
    df_test, 
    date_start = min(df$date),
    date_end = max(df$date)
  )
  
  # Select variables
  df_test <- select(
    df_test,
    date_start,
    date_end,
    p_value = p,
    p_stars, 
    direction,
    slope, 
    intercept,
    slope_lower = lower,
    slope_upper = upper
  )
  
  return(df_test)
  
}


quiet <- function (x) {
  sink(tempfile())
  on.exit(sink())
  invisible(force(x))
}


#' Function to calculate a trend vector for a time series. 
#' 
#' @param df Input data frame. 
#' 
#' @param variable Variable to test. 
#' 
#' @param deseason Should the time series be deseaonsalised before the trend 
#' test is conducted?
#' 
#' @param auto_correlation Should auto correlation be considered in the estimates?
#' 
#' @param df_trend An existing trend data frame from 
#' \code{\link{theil_sen_trend_test}}. An optional argument. 
#' 
#' @seealso \code{\link{theil_sen_trend_test}}
#' 
#' @return Numeric vector with the length of \code{nrow(df)}. 
#' 
#' @author Stuart K. Grange
#'
#' @export
theil_sen_trend_vector <- function(df, variable = "value", deseason = FALSE, 
                                   auto_correlation = FALSE, df_trend = NA) {
  
  if (!"date" %in% names(df))
    stop("`df` must have a `date` variable...", call. = FALSE)
  
  # Get date vector
  date <- as.numeric(df$date)
  
  # Do trend test if needed
  if (is.na(df_trend[1])) {
    
    df_trend <- theil_sen_trend_test(
      df, 
      variable = variable,
      deseason = deseason,
      auto_correlation = auto_correlation
    )
    
  }
  
  # Equation of a staight line
  # Coefficent is number of seconds in a year
  value_predict <- df_trend$slope / 31536000 * date + df_trend$intercept
  
  return(value_predict)
  
}

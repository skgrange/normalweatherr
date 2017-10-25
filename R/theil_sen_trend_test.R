#' Function to test a time series with the Theil-Sen estimator. 
#' 
#' @param df Data frame. 
#' 
#' @param variable Variable name to test. 
#' 
#' @param deseason Should the time series be deseaonsalised before the trend test
#' is conducted. 
#' 
#' @param auto_correlation Should auto correlation be considered in the estimates?
#' 
#' @seealso \link{\code{TheilSen}}
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
  df_test <- dplyr::filter(df_test, is.finite(conc))
  
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

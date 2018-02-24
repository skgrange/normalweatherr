#' Function to use a test set for predictions within a 
#' \strong{normalweatherr_model} object. 
#' 
#' @author Stuart K. Grange
#' 
#' @param A \strong{normalweatherr_model} object. 
#' 
#' @return Data frame. 
#' 
#' @export
predict_using_test_set <- function(list_model) {
  
  if (!class(list_model) == "normalweatherr_model")
    stop("Input must be a `normalweatherr_model`...", call. = FALSE)
  
  # Predict
  value_predict <- enlightenr::make_prediction(
    list_model$model, 
    list_model$testing
  )
  
  # Build prediction data frame
  df <- data.frame(
    value = list_model$testing$value,
    value_predict = value_predict,
    stringsAsFactors = FALSE
  )
  
  # Add delta
  df$value_delta <- abs(df$value - df$value_predict)
  
  # Add row counts for time series plotting
  df$row_number <- seq(1, nrow(df))
  df <- dplyr::select(df, row_number, dplyr::everything())
  
  return(df)
  
}

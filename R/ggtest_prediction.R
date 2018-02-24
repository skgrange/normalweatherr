#' Function to use the testing set in a \strong{normalweatherr_model} object and
#' plot the values and predicted values as a hex-plot. 
#' 
#' @param list_model A \strong{normalweatherr_model} object. 
#' 
#' @return Invisible, a plot.   
#' 
#' @author Stuart K. Grange
#' 
#' @export
ggtest_prediction <- function(list_model) {
  
  # Predict
  df <- predict_using_test_set(list_model)
  
  # For axes
  max_value <- max(df[, -1], na.rm = TRUE)
  
  # Plot
  plot <- ggplot2::ggplot(df, ggplot2::aes(value, value_predict)) + 
    ggplot2::geom_hex(show.legend = FALSE, na.rm = TRUE) + 
    ggplot2::theme_minimal() + 
    ggplot2::geom_abline(slope = 1, intercept = 0) + 
    ggplot2::ylim(0, max_value) + 
    ggplot2::xlim(0, max_value) +
    ggplot2::coord_fixed() +
    viridis::scale_fill_viridis(
      option = "inferno",
      begin = 0.3,
      end = 0.8
    )
  
  return(plot)
  
}

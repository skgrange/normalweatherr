#' Function to split an input data frame into training and testing sets. 
#' 
#' Use \code{set.seed} to ensure the splitting of the training and testing sets
#' is reproducable. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame containing \code{date} and \code{value} variables. 
#' \code{df} will usually be prepared with \code{\link{add_date_variables}}. 
#' 
#' @param fraction Fraction of observations to form the training set. Default is
#' \code{0.8} for an 80/20 \% split for training and testing sets. 
#' 
#' @return A named list containing two data frames: \code{training} and 
#' \code{testing} with the class \code{normalweatherr_data}. 
#' 
#' @seealso \code{\link{set.seed}}, \code{\link{add_date_variables}},
#'  \code{\link{calculate_model}}, \code{\link{normalise_for_meteorology}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Make modelling reproducable
#' set.seed(123)
#' 
#' # Prepare data for modelling
#' list_input_data <- split_input_data(data_swiss_daily)
#' 
#' }
#' 
#' @export
split_input_data <- function(df, impute = TRUE, fraction = 0.8) {
  
  # Check data frame input, df_tbl will not simplify when [, ] are used
  if (any(grepl("tbl", class(df)))) df <- data.frame(df)
  
  if (!any(grepl("value", names(df)))) 
    stop("Data must contain a `value` variable.", call. = FALSE)
  
  # Sample to create test and training data
  random_rows <- random_rows(df, fraction = fraction)
  df_training <- df[random_rows, ]
  df_testing <- df[-random_rows, ]
  
  # Create named list
  list_data <- list(
    training = df_training,
    testing = df_testing
  )
  
  # Give class
  class(list_data) <- "normalweatherr_data"
  
  return(list_data)
  
}

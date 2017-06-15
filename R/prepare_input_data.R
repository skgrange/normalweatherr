#' Function to prepare meteorological and air quality data for modelling with
#' the \strong{normalweatherr} package. 
#' 
#' \code{prepare_input_data} will check variable names, transform the parsed 
#' \code{date} variable into other variables, impute missing data, make correct
#' data types, and split the input data set into training and testing sets. 
#' 
#' Use \code{set.seed} to ensure the splitting of the training and testing sets
#' is reproducable. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame containing \code{date} and \code{value} variables. 
#' 
#' @param impute Should missing values be imputed?
#' 
#' @param fraction Fraction of observations to form the training set. Default is
#' \code{0.8} for an 80/20 \% split for training and testing sets. 
#' 
#' @return A named list containing two data frames: \code{training} and 
#' \code{testing} with the class \code{normalweatherr_data}. 
#' 
#' @seealso \code{\link{set.seed}}, \code{\link{calculate_model}}, 
#' \code{\link{normalise_for_meteorology}}
#' 
#' @examples 
#' \dontrun{
#' 
#' # Make modelling reproducable
#' set.seed(123)
#' 
#' # Prepare data for modelling
#' list_input_data <- prepare_input_data(data_swiss_daily)
#' 
#' }
#' 
#' @export
prepare_input_data <- function(df, impute = TRUE, fraction = 0.8) {
  
  # Check data frame input, df_tbl will not simplify when [, ] are used
  if (any(grepl("tbl", class(df)))) df <- data.frame(df)
  
  # Check variables
  names <- names(df)
  
  if (!any(grepl("value", names))) 
    stop("Data must contain a `value` variable.", call. = FALSE)
  
  if (!any(grepl("date", names))) 
    stop("Data must contain a `date` variable.", call. = FALSE)
  
  if (!any(grepl("POSIXct", class(df$date))))
    stop("`date` variable needs to be a parsed date (POSIXct).", call. = FALSE)
  
  # Add variables if they do not exist
  # Add date variables
  if (!any(grepl("date_unix", names))) 
    df[, "date_unix"] <- as.numeric(df[, "date"])
  
  if (!any(grepl("week", names)))
    df[, "week"] <- lubridate::week(df[, "date"])
  
  if (!any(grepl("weekday", names)))
    df[, "weekday"] <- wday_monday(df[, "date"])
  
  if (!any(grepl("hour", names)))
    df[, "hour"] <- lubridate::hour(df[, "date"])
  
  if (!any(grepl("month", names)))
    df[, "month"] <- lubridate::month(df[, "date"])
  
  if (!any(grepl("day_julian", names)))
    df[, "day_julian"] <- lubridate::yday(df[, "date"])
  
  if (impute) {
    
    # Impute numeric variables
    index <- sapply(df, function (x) is.numeric(x) | is.integer(x))
    
    # Median
    df[index] <- lapply(df[index], function(x) 
      ifelse(is.na(x), median(x, na.rm = TRUE), x))
    
  }
  
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

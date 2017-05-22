#' Function to prepare meteorological and air quality data for modelling. 
#' 
#' \code{prepare_input_data} will check variable names, transform the parsed 
#' \code{date} variable into other variables, impute missing data, and split
#' the data set into the training and testing sets. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame containing \code{date} and \code{value} variables. 
#' 
#' @return A named list containing two data frames with the class 
#' \code{normalweatherr_data}. 
#' 
#' @export
prepare_input_data <- function(df) {
  
  # Check input
  names <- names(df)
  
  if (!any(grepl("value", names))) 
    stop("Data must contain a `value`` variable.", call. = FALSE)
  
  if (!any(grepl("date", names))) 
    stop("Data must contain a `date`` variable.", call. = FALSE)
  
  if (!any(grepl("POSIXct", class(df$date))))
    stop("`date` variable needs to be a parsed date (POSIXct).", call. = FALSE)
  
  # Add date variables
  if (!any(grepl("date_unix", names))) 
    df[, "date_unix"] <- as.numeric(df[, "date"])
  
  if (!any(grepl("week", names)))
    df[, "week"] <- lubridate::week(df[, "date"])
  
  if (!any(grepl("weekday", names)))
    df[, "weekday"] <- wday_monday(df[, "date"])
  
  if (!any(grepl("hour", names)))
    df[, "hour"] <- lubridate::hour(df[, "date"])
  
  # Impute numeric variables
  index <- sapply(df, function (x) is.numeric(x) | is.integer(x))
  
  # Median
  df[index] <- lapply(df[index], function(x) 
    ifelse(is.na(x), median(x, na.rm = TRUE), x))
  
  # Sample to create test and training data
  random_rows <- random_rows(df)
  df_training <- df[random_rows, ]
  df_testing <- df[-random_rows, ]
  
  # Create list
  list_data <- list(
    training = df_training,
    testing = df_testing
  )
  
  # Give class
  class(list_data) <- "normalweatherr_data"
  
  return(list_data)
  
}


#' Function to model concentration based on meteorological and time variables. 
#' 
#' @author Stuart K. Grange
#' 
#' @param list_input_data \code{normalweatherr_data} list containing the prepared
#' training and testing sets. 
#' 
#' @param variables Variables to include in the random forest model. 
#' 
#' @param ntree Number of trees to grow for the random forest model. Set 
#' \code{ntree} to a smaller integer for testing. 
#' 
#' @param mtry Number of variables randomly sampled for splitting the random 
#' forest decision tree. 
#' 
#' @param nodesize Minimum size of terminal nodes for the random forest model. 
#' 
#' @param verbose Should the random forest model print progress. 
#' 
#' @param output Directory to export the model object as an \code{.rds} file. 
#' If not used, the model will not be exported to disc. 
#' 
#' @return Named list containing two data frames and a model object with the 
#' class \code{normalweatherr_model}. 
#' 
#' @export
calculate_model <- function(
  list_input_data, 
  variables = c("temp", "rh", "ws", "wd", "date_unix", "week", "weekday", "hour"),
  ntree = 200,
  mtry = 3, 
  nodesize = 3, 
  verbose = TRUE,
  output = NA
  ) {
  
  if (!class(list_input_data) == "normalweatherr_data") 
    stop("Not of correct class.", call. = FALSE)
  
  # Get pieces
  df_training <- list_input_data$training
  df_testing <- list_input_data$testing

  # Add value too
  variables <- c("value", variables)  
  
  # Select the variables
  df_training <- df_training[, variables]
  df_testing <- df_testing[, variables]
  
  # For rf progress
  do.trace <- ifelse(verbose, 2, FALSE)
  
  # Get start time file for export
  date_start <- format(lubridate::now(), usetz = TRUE)
  date_start <- stringr::str_replace_all(date_start, " |:|-|/", "_")
  
  # Model
  list_model <- randomForest::randomForest(
    value ~ ., 
    data = df_training,
    na.action = na.omit,
    do.trace = do.trace, 
    keep.forest = TRUE, 
    importance = TRUE,
    mtry = mtry, 
    nodesize = nodesize, 
    ntree = ntree
  )
  
  # Build return
  list_model <- list(
    data_training = df_training,
    data_testing = df_testing,
    model = list_model
  )
  
  # Give class
  class(list_model) <- "normalweatherr_model"
  
  # Export
  if (!is.na(output[1])) {
    
    # Create directory if needed
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    
    # Build file name
    file_output <- stringr::str_c(date_start, "_normalweatherr_model.rds")
    file_output <- file.path(output, file_output)
    
    # Export as an rds object
    saveRDS(list_model, file_output)
    
  }
  
  return(list_model)
  
}


#' Function to normalise a concentration variable based on "average" 
#' meteorological conditions. 
#' 
#' @param list_model A \code{randomForest} model object, typically created with
#' \code{\link{calculate_model}}. 
#' 
#' @param df Data frame to use for prediction. Typically created with 
#' \code{\link{prepare_input_data}}. 
#' 
#' @param variables Variable to randomly sample. 
#' 
#' @param n Number of times to sample \code{df}. 
#' 
#' @param replace Should \code{variables} be sampled with replacement? 
#' 
#' @param output Directory to export the model object as an \code{.rds} file. 
#' If not used, the model will not be exported to disc. 
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @export
normalise_for_meteorology <- function(
  list_model, 
  df, 
  variables = c("wd", "ws", "temp", "rh", "hour", "weekday", "week"),
  n = 100, 
  replace = FALSE,
  output = NA
  ) {
  
  # Check input
  if (!any(grepl("randomForest", class(list_model)))) 
    stop("Model needs to be of class 'randomForest'.", call. = FALSE)
  
  # Get start time file for export
  date_start <- format(lubridate::now(), usetz = TRUE)
  date_start <- stringr::str_replace_all(date_start, " |:|-|/", "_")
  
  # Do in parallel
  df <- plyr::ldply(1:n, function(x) 
    randomly_sample_meteorology(
      list_model, 
      df, 
      variables,
      replace = replace
    ), 
    .parallel = TRUE) %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(value_predict = mean(value_predict, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    data.frame()
  
  # Export
  if (!is.na(output[1])) {
    
    # Create directory if needed
    dir.create(output, recursive = TRUE, showWarnings = FALSE)
    
    # Build file name
    file_output <- stringr::str_c(date_start, "_normalweatherr_normalised.rds")
    file_output <- file.path(output, file_output)
    
    # Export as an rds object
    saveRDS(df, file_output)
    
  }
  
  # Free
  gc()
  
  return(df)
  
}


# No export
randomly_sample_meteorology <- function(
  list_model, 
  df, 
  variables = c("wd", "ws", "temp"),
  replace
  ) {
  
  # Randomly sample observations
  n_rows <- nrow(df)
  index_rows <- sample(1:n_rows, replace = replace)
  
  # Transform data frame to include sampled variables
  df[variables] <- lapply(df[variables], function(x) x[index_rows])
  
  value_predict <- unname(predict(list_model, df))
  
  if (class(value_predict) == "matrix") value_predict <- value_predict[, 1]
  
  # Build data frame of predictions
  df <- data.frame(
    date = df$date,
    value_predict = value_predict
  )
  
  return(df)
  
}


# from deweather package
## randomly sample from original data
# doPred <- function(mydata, mod, metVars) {
#   
#   ## random samples 
#   n <- nrow(mydata) 
#   id <- sample(1 : n, n, replace = FALSE)
#   
#   ## new data with random samples
#   mydata[metVars] <- lapply(mydata[metVars], function (x) x[id])
#   
#   prediction <- predict.gbm(mod, mydata, 1000)
#   prediction <- data.frame(date = mydata$date, pred = prediction)
#   
#   return(prediction)
#   
# }



#' Function to detect breakpoints in a data frame. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame to detect breakpoints in. 
#' 
#' @return Data frame.
#' 
#' @export
detect_breakpoints <- function(df) {
  
  # Check
  if (!any(grepl("date", names(df)))) 
    stop("Data must contain a `date`` variable.", call. = FALSE)
  
  if (!any(grepl("POSIXct", class(df$date))))
    stop("`date` variable needs to be a parsed date (POSIXct).", call. = FALSE)
  
  # Switch a common variable name
  names(df) <- ifelse(names(df) == "value_predict", "value", names(df))
  
  # Do
  breakpoints <- strucchange::breakpoints(value ~ date, data = df)
  
  # Get the dates
  df <- df[breakpoints$breakpoints, c("date")]
  
  # Return data frame
  return(data.frame(df))
  
}

#' Function to prepare meteorological and air quality data for modelling with
#' the \strong{normalweatherr} package. 
#' 
#' \code{prepare_input_data} will check variable names, transform the parsed 
#' \code{date} variable into other variables, impute missing data, make correct
#' data types, and split the input data set into training and testing sets. 
#' 
#' @author Stuart K. Grange
#' 
#' @param df Data frame containing \code{date} and \code{value} variables. 
#' 
#' @param fraction Fraction of observations to form the training set. Default is
#' \code{0.8} for an 80/20 \% split for training and testing sets. 
#' 
#' @return A named list containing two data frames with the class 
#' \code{normalweatherr_data}. 
#' 
#' @export
prepare_input_data <- function(df, fraction = 0.8) {
  
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
  
  # Also data types here
  if (!any(grepl("weekday", names))) {
    
    df[, "weekday"] <- wday_monday(df[, "date"])
    df[, "weekday"] <- as.factor(df[, "weekday"])
    
  }
  
  if (!any(grepl("hour", names))) {
    
    df[, "hour"] <- lubridate::hour(df[, "date"])
    df[, "hour"] <- as.factor(df[, "hour"])
    
  }
  
  # Impute numeric variables
  index <- sapply(df, function (x) is.numeric(x) | is.integer(x))
  
  # Median
  df[index] <- lapply(df[index], function(x) 
    ifelse(is.na(x), median(x, na.rm = TRUE), x))
  
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


#' Function to model concentration based on meteorological and time variables. 
#' 
#' @author Stuart K. Grange
#' 
#' @param list_input_data \code{normalweatherr_data} list containing the prepared
#' training and testing sets; produced by \code{\link{prepare_input_data}}. 
#' 
#' @param variables Variables to include in the model. 
#' 
#' @param ntree Number of trees to grow for the random forest model. Set 
#' \code{ntree} to a smaller integer for testing. 
#' 
#' @param mtry Number of variables randomly sampled for splitting the random 
#' forest decision tree. 
#' 
#' @param nodesize Minimum size of terminal nodes for the random forest model. 
#' 
#' @param verbose Should the random forest model print progress? 
#' 
#' @param output File name to export the model object as an \code{.rds} file. 
#' If not used, the model will not be exported to disc. Directories will be 
#' created if necessary.  
#' 
#' @return Named list containing two data frames and a model object with the 
#' class \code{normalweatherr_model}. 
#' 
#' @seealso \code{\link{prepare_input_data}}
#' 
#' @export
calculate_model <- function(
  list_input_data, 
  variables = c("temp", "rh", "ws", "wd", "date_unix", "week", "weekday", "hour"),
  model = "random_forest",
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
  
  # For export
  if (!is.na(output[1])) {
    
    # Strip file name
    output_directory <- dirname(output)
    
    # Create if needed
    if (!dir.exists(output_directory)) 
      dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    
    # Ensure output is rds
    output <- stringr::str_split_fixed(basename(output), "\\.", 2)[, 1]
    output <- stringr::str_c(output, ".rds")
    
    # Add path again
    output <- file.path(output_directory, output)
     
  }
  
  if (grepl("forest", model, ignore.case = TRUE)) {
    
    # For rf progress
    do.trace <- ifelse(verbose, 2, FALSE)
    
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
    
  } else if (grepl("vector", model, ignore.case = TRUE)) {
    
    list_model <- kernlab::ksvm(
      value ~ ., 
      data = df_training,
      kernel = "rbfdot", 
      C = 1, 
      kpar = list(sigma = 1)
    )
    
  } else {
    
    stop("'model' not recognised.", call. = FALSE)
    
  }
  
  # Build return
  list_model <- list(
    data_training = df_training,
    data_testing = df_testing,
    model = list_model
  )
  
  # Give class
  class(list_model) <- "normalweatherr_model"
  
  # Export
  if (!is.na(output[1])) saveRDS(list_model, output)
  
  return(list_model)
  
}


#' Function to normalise a concentration variable based on "average" 
#' meteorological conditions. 
#' 
#' @param list_model A \code{normalweatherr_model} model object, created with 
#' \code{\link{calculate_model}}. 
#' 
#' @param df Data frame to use for prediction. Created with 
#' \code{\link{prepare_input_data}}. 
#' 
#' @param variables Variable to randomly sample. 
#' 
#' @param n Number of times to sample \code{df}. 
#' 
#' @param replace Should \code{variables} be sampled with replacement? 
#' 
#' @param output File name to export the model object as an \code{.rds} file. 
#' If not used, the model will not be exported to disc. Directories will be 
#' created if necessary.  
#' 
#' @author Stuart K. Grange
#' 
#' @return Data frame. 
#' 
#' @seealso \code{\link{prepare_input_data}}, \code{\link{calculate_model}}
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
  
  # Get model type
  model_type <- class(list_model)[1]
  
  # Check input
  if (!any(grepl("randomForest|ksvm", model_type))) 
    stop("Model needs to be of class 'randomForest' or 'ksvm'.", call. = FALSE)
  
  # For export
  if (!is.na(output[1])) {
    
    # Strip file name
    output_directory <- dirname(output)
    
    # Create if needed
    if (!dir.exists(output_directory)) 
      dir.create(output_directory, recursive = TRUE, showWarnings = FALSE)
    
    # Ensure output is rds
    output <- stringr::str_split_fixed(basename(output), "\\.", 2)[, 1]
    output <- stringr::str_c(output, ".rds")
    
    # Add path again
    output <- file.path(output_directory, output)
    
  }
  
  # Do in parallel
  df <- plyr::ldply(1:n, function(x) 
    randomly_sample_meteorology(
      list_model, 
      df, 
      variables,
      replace = replace,
      model = model_type
    ), 
    .parallel = TRUE) %>% 
    dplyr::group_by(date) %>% 
    dplyr::summarise(value_predict = mean(value_predict, na.rm = TRUE)) %>% 
    dplyr::ungroup() %>% 
    data.frame()
  
  # Export
  if (!is.na(output[1])) saveRDS(list_model, output)
  
  # Free
  gc()
  
  return(df)
  
}


# No export
randomly_sample_meteorology <- function(
  list_model, 
  df, 
  variables = c("wd", "ws", "temp"),
  replace,
  model
  ) {
  
  # Randomly sample observations
  n_rows <- nrow(df)
  index_rows <- sample(1:n_rows, replace = replace)
  
  # Transform data frame to include sampled variables
  df[variables] <- lapply(df[variables], function(x) x[index_rows])
  
  # Different precition logic
  if (model == "randomForest.formula") {
    
    # Seems to be generic
    value_predict <- unname(predict(list_model, df))
    
  } else if (model == "ksvm") {
    
    # Not generic and returns a matrix
    value_predict <- unname(kernlab::predict(list_model, df))[, 1]
    
  } else {
    
    stop("Model not recognised.", call. = FALSE)
    
  }
  
  # Build data frame of predictions
  df <- data.frame(
    date = df$date,
    value_predict = value_predict
  )
  
  return(df)
  
}


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
    stop("Data must contain a `date`` variable.", call. = FALSE)
  
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

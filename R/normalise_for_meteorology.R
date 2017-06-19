#' Function to normalise a concentration variable based on "average" 
#' meteorological conditions. 
#' 
#' @param list_model A \code{normalweatherr_model} model object, created with 
#' \code{\link{calculate_model}}. 
#' 
#' @param df Data frame to use for prediction. Created with 
#' \code{\link{prepare_input_data}}. 
#' 
#' @param variables Variables to include in the randomly sample. 
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
normalise_for_meteorology <- function(list_model, df, variables, n = 100, 
                                      replace = FALSE, output = NA) {
  
  # Get model type
  model_type <- class(list_model)[1]
  
  # Check input
  if (!any(grepl("randomForest|ksvm|gbm|gam", model_type))) {
    
    stop(
      "Model needs to be of class `randomForest`, `ksvm`, `gbm`, or `gam`.", 
      call. = FALSE
    )
    
  }
  
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
    group_by(date) %>% 
    summarise(value_predict = mean(value_predict, na.rm = TRUE)) %>% 
    ungroup() %>% 
    data.frame()
  
  # Export
  if (!is.na(output[1])) saveRDS(df, output)
  
  # Free
  gc()
  
  return(df)
  
}


# No export
randomly_sample_meteorology <- function(list_model, df, variables, replace, 
                                        model) {
  
  # Use date unix if date does not exist in input data frame
  if ("date_unix" %in% names(df) & !"date" %in% names(df))
    df[, "date"] <- df[, "date_unix"]
  
  # Randomly sample observations
  n_rows <- nrow(df)
  index_rows <- sample(1:n_rows, replace = replace)
  
  # Transform data frame to include sampled variables
  df[variables] <- lapply(df[variables], function(x) x[index_rows])
  
  # Different precition logic
  if (model %in% c("randomForest.formula", "gam")) {
    
    # Seems to be generic
    value_predict <- unname(predict(list_model, df))
    
  } else if (model == "ksvm") {
    
    # Not generic and returns a matrix
    value_predict <- unname(kernlab::predict(list_model, df))[, 1]
    
  } else if (model == "gbm") {
    
    # Use a vector but needs an extra argument, comes from model object
    value_predict <- gbm::predict.gbm(
      list_model, 
      df, 
      n.trees = length(list_model$trees)
    )
    
  } else {
    
    stop("Model not recognised.", call. = FALSE)
    
  }
  
  # Build data frame of predictions
  df <- data.frame(
    date = df[, "date"],
    value_predict = value_predict
  )
  
  return(df)
  
}


# ## randomly sample from original data
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

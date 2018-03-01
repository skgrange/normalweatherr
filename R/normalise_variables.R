#' Function to normalise for meteorology for many variables. 
#'
#' @author Stuart K. Grange
#' 
#' @param df Data frame containing a \code{date} variable. 
#' 
#' @param variables Variables in \code{df} to normalise for meteorology.
#' 
#' @param variables_explanatory Variables to include in the model, \emph{i.e.} 
#' the predictors. 
#' 
#' @param model Model type to use. Default is \code{"rf"} for random forest. See 
#' \code{\link{calculate_model}} for details. 
#' 
#' @param ntree Number of trees to grow for "rf" or "gbm" models. Default is low
#' for testing purposes, increase for usable models. 
#' 
#' @param mtry Number of variables randomly sampled for splitting the \code{"rf"}
#' decision tree.
#' 
#' @param nodesize Minimum size of terminal nodes for the "rf" model.
#' 
#' @param n_predict Number of times to sample input data and predict using the 
#' model. 
#' 
#' @param verbose Should the function give messages? 
#'
#' @return Named list with split input data, model performance data, and 
#' normalised data. 
#' 
#' @seealso \code{\link{add_date_variables}}, \code{\link{split_input_data}}, 
#' \code{\link{calculate_model}}, \code{\link{normalise_for_meteorology}}
#'
#' @export
normalise_variables <- function(df, variables, variables_explanatory, 
                                model = "rf", ntree = 10, mtry = 3, nodesize = 3,
                                n_predict = 10, verbose = TRUE) {
  
  # Check
  if (anyDuplicated(variables) != 0) stop("Duplicated `variables`...")
  
  # Variables is plural here, but not in worker
  list_models <- purrr::map(
    .x = variables,
    .f = ~normalise_variables_worker(
      df = df,
      variable = .x,
      variables_explanatory = variables_explanatory,
      model = model,
      ntree = ntree,
      mtry = 3,
      nodesize = nodesize,
      n_predict = n_predict,
      verbose = verbose
    )
  )
  
  # Give names
  names(list_models) <- variables
  
  # Give class
  list_models <- purrr::modify(
    list_models, function(x) {
      class(x) <- "normalweatherr_model"
      return(x)
    }
  )
  
  return(list_models)
  
}


normalise_variables_worker <- function(df, variable, variables_explanatory, 
                                       model, ntree, mtry, nodesize, n_predict, 
                                       verbose) {
  
  # Check
  if (!"site" %in% names(df)) 
    stop("Input data frame must contain a `site` variable...", call. = FALSE)
  
  # Get site
  site <-  df$site[1]
  
  # Rename variable to the generic value for modelling
  names(df) <- ifelse(names(df) == variable, "value", names(df))
  
  # Make a named list
  list_input <- split_input_data(df)
  
  # Build the predictive model
  if (verbose) message(str_date_formatted(), ": Modelling `", variable, "`...")
  
  list_model <- calculate_model(
    list_input,
    variables = variables_explanatory,
    model = model,
    ntree = ntree,
    mtry = mtry,
    nodesize = nodesize,
    verbose = verbose
  )
  
  # Get performance statistics for model
  if (verbose) 
    message(str_date_formatted(), ": Calculating model performance measures...")
  
  df_performance <- data.frame(
    site = site,
    model = model,
    variable = variable,
    mse = enlightenr::extract_rf_mse(list_model$model),
    r_squared = enlightenr::extract_rf_r_squared(list_model$model),
    stringsAsFactors = FALSE
  )
  
  # Normalise for meteorology
  if (verbose) message(str_date_formatted(), ": Predicting `", variable, "`...")
  
  df_normalised <- normalise_for_meteorology(
    list_model$model, 
    df, 
    variables = setdiff(variables_explanatory, "date_unix"),
    n = n_predict,
    output = NA
  )
  
  if (verbose) message(str_date_formatted(), ": Cleaning up output.....")
  
  # Add extras to normalised data frame
  df_normalised$site <- site
  df_normalised$variable <- variable
  
  # Arrange variable
  df_normalised <-  df_normalised[, c("date", "site", "variable", "value_predict")]
  
  # Build the list return, append
  list_model <- c(
    list_model, 
    "model_performance" = list(df_performance),
    "normalised" = list(df_normalised)
  )
  
  # Print a message to the user
  if (nodesize <= 10) {
    
    message(
      str_date_formatted(), 
      ": `nodesize` is very low, increase for better performance..."
    )
    
  }
  
  if (n_predict <= 50) {
    
    message(
      str_date_formatted(), 
      ": `n_predict` is very low, increase for better performance...\n"
    )
    
  }
  
  return(list_model)
  
}


# Pulled from thereadr
str_date_formatted <- function(date = NA, time_zone = TRUE, 
                               fractional_seconds = TRUE) {
  
  # Get date if not supplied
  if (is.na(date)[1]) date <- lubridate::now(tz = Sys.timezone())
  
  # Format string
  format_date <- ifelse(
    fractional_seconds, 
    "%Y-%m-%d %H:%M:%OS3", 
    "%Y-%m-%d %H:%M:%S"
  )
  
  # Format
  x <- format(date, format = format_date, usetz = time_zone)
  
  return(x)
  
}

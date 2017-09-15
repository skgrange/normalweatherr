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
#' @param progress Type of progress bar to display. 
#'
#' @return Named list with split input data, model performance data, and 
#' normalised data. 
#'
#' @export
normalise_variables <- function(df, variables, variables_explanatory, 
                                progress = "time") {
  
  # Variables is plural here, but not in worker
  plyr::llply(
    variables,
    function(x) normalise_variables_worker(
      df = df,
      variable = x,
      variables_explanatory = variables_explanatory
    ),
    .progress = progress
  )
  
}


normalise_variables_worker <- function(df, variable, variables_explanatory, 
                                       model = "rf") {
  
  # Check
  if (!"site" %in% names(df)) 
    stop("Input data frame must contain a `site` variable...", call. = FALSE)
  
  # Get site
  site <-  df$site[1]
  
  # Rename variable to the generic value for modelling
  names(df) <- ifelse(names(df) == variable, "value", names(df))
  
  # Make a named list
  list_input <- split_input_data(df)
  
  message(stringr::str_c("Modelling `", variable, "`..."))
  
  list_model <- calculate_model(
    list_input,
    variables = variables_explanatory,
    model = model,
    ntree = 200,
    verbose = FALSE
  )
  
  # Get performance statistics for model
  df_performance <- data.frame(
    site = site,
    model = model,
    variable = variable,
    mse = enlightenr::extract_rf_mse(list_model$model),
    r_squared = enlightenr::extract_rf_r_squared(list_model$model),
    stringsAsFactors = FALSE
  )
  
  # Normalise for meteorology
  message(stringr::str_c("Predicting `", variable, "`..."))
  
  df_normalised <- normalise_for_meteorology(
    list_model$model, 
    df, 
    variables = setdiff(variables_explanatory, "date_unix"),
    n = 1000,
    output = NA
  )
  
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
  
  return(list_model)
  
}

#' Function to model pollutant concentrations based on meteorological and time
#' variables. 
#' 
#' To-do: Support for models other than random forest is pretty poor at the 
#' moment, this needs to be fixed. 
#' 
#' @param list_input_data \code{normalweatherr_data} list containing the prepared
#' training and testing sets; produced by \code{\link{split_input_data}}. 
#' 
#' @param variables Variables to include in the model. 
#' 
#' @param model Model type to use. Can be: \code{"rf"}, \code{"svm"}, \code{"gam"}, 
#' \code{"gbm"} for random forest, support vector machines, generalised additive
#' models, and generalised boosted regression models respectively. 
#' 
#' @param ntree Number of trees to grow for \code{"rf"} or \code{"gbm"} models. 
#' Set \code{ntree} to a smaller integer for testing. 
#' 
#' @param mtry Number of variables randomly sampled for splitting the \code{"rf"}
#' decision tree. 
#' 
#' @param nodesize Minimum size of terminal nodes for the \code{"rf"} model. 
#' 
#' @param k For \code{"gam"} models, number of knots to use for the smooth 
#' functions. 
#' 
#' @param bs For \code{"gam"} models, the smoothing basis to use. Default is 
#' \code{"tp"} and represents a thin plate regression spline. 
#' 
#' @param verbose Should the models print progress if this is supported? 
#' 
#' @param output File name to export the model object as an \code{.rds} file. 
#' If not used, the model will not be exported to disc. Directories will be 
#' created if necessary. 
#' 
#' @return Named list containing two data frames (contents of 
#' \code{list_input_data}) and a model object with the class 
#' \code{normalweatherr_model}. 
#' 
#' @author Stuart K. Grange
#' 
#' @examples 
#' \dontrun{
#' 
#' # Calculate a random forest model
#' list_model <- calculate_model(
#'   list_input_data = list_input_data, 
#'   variables = variables, 
#'   model = "rf",
#'   ntree = 200,
#'   output = NA,
#'   mtry = 5,
#'   nodesize = 1,
#'   verbose = FALSE
#' )
#' 
#' }
#' 
#' @seealso \code{\link{prepare_input_data}}, 
#' \code{\link{normalise_for_meteorology}}
#' 
#' @export
calculate_model <- function(list_input_data, variables, model = "rf", ntree = NA,
                            mtry = 3, nodesize = 3, verbose = TRUE, output = NA) {
  
  # Check inputs
  if (!class(list_input_data) == "normalweatherr_data") 
    stop("Input is not of correct class.", call. = FALSE)
  
  # Parse argument for logic
  model <- stringr::str_to_lower(model)
  
  # Defaults for the different modeling methods
  if (is.na(ntree)) {
    
    if (model == "rf") ntree <- 200
    if (model == "gbm") ntree <- 1000
    
  }
  
  # Get pieces of list, easier to reference like this
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
  
  if (model == "rf") {
    
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
    
  } else if (model == "svm") {
    
    list_model <- kernlab::ksvm(
      value ~ ., 
      data = df_training,
      type = "eps-bsvr",
      kernel = "rbfdot", 
      C = 5, 
      kpar = list(sigma = 1),
      epsilon = 0.1
    )
    
  } else if (model == "gbm") {
    
    # Use values from David's dweather package for now
    list_model <- gbm::gbm(
      value ~.,
      data = df_training,
      distribution = "gaussian",
      n.trees = ntree,
      shrinkage = 0.1, 
      interaction.depth = 10,
      bag.fraction = 0.7,
      train.fraction = 1,
      n.minobsinnode = 10,
      keep.data = TRUE,
      verbose = verbose
    )
    
  } else if (model == "gam")  {
    
    # Get inputs
    smooth_terms <- variables[variables != "value"]
    
    # Build smooth terms for model
    smooth_terms <- sapply(
      smooth_terms, 
      function(x) 
        build_smooth_terms(x, k = 14 ,bs = "tp"), 
      USE.NAMES = FALSE
    )
    
    smooth_terms <- stringr::str_c(smooth_terms, collapse = " + ")
    
    # Add the dependent variable, value
    formula <- stringr::str_c(
      "value ~ ", 
      smooth_terms
    )
    
    # Do
    list_model <- mgcv::gam(
      as.formula(formula),
      data = df_training,
      na.action = na.omit
    )
    
  } else {
    
    stop("`model` not recognised.", call. = FALSE)
    
  }
  
  # Build return
  list_model <- list(
    training = df_training,
    testing = df_testing,
    model = list_model
  )
  
  # Give class
  class(list_model) <- "normalweatherr_model"
  
  # Export
  if (!is.na(output[1])) saveRDS(list_model, output)
  
  return(list_model)
  
}


build_smooth_terms <- function(x, k, bs) {
  
  if (x == "weekday") {
    
    # Weekday should be an ordered factor, therefore no smooth term
    x <- "weekday"
    
    # # Different number of knots needed here due to fewer levels
    # x <- stringr::str_c(
    #   "s(", x, ", bs = '", bs, "', k = ", 7, ")"
    # )
    
  } else {
    
    x <- stringr::str_c(
      "s(", x, ", bs = '", bs, "', k = ", k, ")"
    )
    
  }
  
  return(x)
  
}

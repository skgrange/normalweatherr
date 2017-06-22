#' Function to get weekday number from a date where \code{1} is Monday and 
#' \code{7} is Sunday. 
#' 
#' @author Stuart K. Grange
#' 
#' @param x Date vector.
#' 
#' @param as.factor Should the return be a factor? 
#' 
#' @return Numeric vector.
#' 
wday_monday <- function(x, as.factor = FALSE) {
  
  x <- lubridate::wday(x)
  x <- x - 1
  x <- ifelse(x == 0, 7, x)
  if (as.factor) x <- factor(x, levels = 1:7, ordered = TRUE)
  return(x)
  
}


#' Function to get a vector of random rows for data frame sampling, usually for
#' creating training and testing data frames for models. 
#' 
#' Use \code{set.seed} before the function to make the random number generation
#' reproducible. 
#' 
#' @param df Data frame to get random rows from. 
#' 
#' @param fraction Fraction of \code{df} to sample. 
#' 
#' @return Integer vector. 
#' 
#' @author Stuart K. Grange
#' 
random_rows <- function(df, fraction = 0.8) {
  
  # Get n
  n <- nrow(df)
  
  # Sample
  rows <- sample(n, round(n * fraction))
  
  return(rows)
  
}


#' Function to register parallel backend. 
#'
#' @author Stuart K. Grange
#' 
#' @return Invisible. 
#'
#' @export
register_cores <- function(cores = NA) {
  
  # Default is n - 1
  if (is.na(cores)) cores <- parallel::detectCores() - 1
  
  # Register
  doMC::registerDoMC(cores)
  
  # No return
  
}


#' @importFrom openair timeAverage
#' @export
openair::timeAverage


mode_average <- function (x, na.rm = FALSE) {
  if (na.rm) x <- na.omit(x)
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

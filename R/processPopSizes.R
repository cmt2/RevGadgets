#' Process Population Sizes
#'
#'
#' Processing the output of a coalescent demographic analysis.
#'
#' For processing the output of a coalescent demographic analysis.
#' processPopSizes() assumes that the the first size parameter (i.e. population_size[1])
#' corresponds to the present. processPopSizes() partly
#' relies on readTrace and produces a list object that can be read by
#' plotPopSizes() to visualize the results. For now, only one log file per
#' parameter type is accepted (i.e. log files from multiple runs must be
#' combined before reading into the function).
#'
#' @param population_size_log (vector of character strings or
#' single character string; "") Path to population sizes log file(s)
#' @param interval_change_points_log (vector of character strings or
#' single character string; "") Path to interval change points log file(s).
#' If not given, a constant process with only one population size is assumed.
#' @param model (string, default: "constant") The demographic model of the intervals.
#' Can be "constant" or "linear".
#' @param burnin (single numeric value; default: 0.25) Fraction of generations to
#'  discard (if value provided is between 0 and 1) or number of generations (if
#'  value provided is greater than 1).
#' @param probs (numeric vector; c(0.025, 0.975)) a vector of length two
#' containing the upper and lower bounds for the confidence intervals.
#' @param summary (string, default: "median") the metric to summarize the
#' posterior distribution, typically "mean" or "median".
#' @param num_grid_points (numeric; default: 100) defines the number of grid points through time for which to
#' evaluate the demographic functions.
#' @param spacing (string, default: "exponential") The spacing of grid points. Can be "exponential" or "equal".
#' Exponentially spaced grid points are dense towards the present and have larger distances towards the past.
#' @param max_age (numeric; default: NULL, i.e. not provided) defines the maximal age up to which the demographic functions should be evaluated.
#' If not provided, it will either be automatically set to 1e5 (in case of a constant process) or
#' to the maximal age provided with the interval_change_points_log.
#' @param min_age (numeric; default: NULL, i.e. not provided) defines the minimal age up to which the demographic functions should be evaluated.
#' If not provided, it will either be automatically set to 1e2 (in case of a constant process) or
#' to the minimal age provided with the interval_change_points_log. Can not be 0 in case of exponential spacing.
#' @param distribution (boolean; default: FALSE) specifies whether the summary data frame will be returned 
#' (distribution = FALSE) or a matrix with distributions of population size for each point on the grid and 
#' with the times of the grid points as row names (distribution = TRUE).
#' @return List object with processed rate and, if applicable, time parameters (if distribution = FALSE). 
#' Matrix object with distributions of population size (if distribution = TRUE). If applicable, one row for each point on the grid, 
#' with the times of the grid points as row names.
#'
#' @export

processPopSizes <- function(population_size_log = "",
                            interval_change_points_log = "",
                            model = "constant",
                            burnin = 0.25,
                            probs = c(0.025, 0.975),
                            summary = "median",
                            num_grid_points = 100,
                            spacing = "exponential",
                            max_age = NULL,
                            min_age = NULL,
                            distribution = FALSE){
  
  if (spacing == "exponential" && !is.null(min_age) && min_age == 0){
    stop("Exponential spacing of grid points can not be combined with a minimal age of 0. Please choose either equal spacing or a different min_age.")
  }
  
  constant_dem = FALSE
  
  if (interval_change_points_log == ""){
    constant_dem = TRUE
  }
  
  if (constant_dem == TRUE){
    pop_size <- readTrace(paths = population_size_log)[[1]]
    pop_sizes <- pop_size[, ncol(pop_size)]
    
    if ( is.null(min_age) ) {
      min_age <- 1E-2
    }
    if ( is.null(max_age) ) {
      max_age <- 1e5
    }
    
    if (spacing == "exponential"){
      x <- exp(seq(log(min_age), log(max_age), length.out = num_grid_points))
    } else if (spacing == "equal"){
      x <- seq(min_age, max_age, length.out = num_grid_points)
    } else {
      stop('Please provide as grid point spacing either "exponential" or "equal".')
    }
    
    if (distribution == TRUE){
      m =  matrix(rep(pop_sizes,each=num_grid_points),nrow=num_grid_points)
    } else {
      summary_size <- apply(as.matrix(pop_sizes), 2, FUN = summary)
      quantiles <- apply(as.matrix(pop_sizes), 2,
                         quantile,
                         probs = probs)
      
      plotdf <- dplyr::tibble(.rows = length(x))
      plotdf["value"] <- rep(summary_size, length(x))
      plotdf["lower"] <- rep(quantiles[1, ], length(x))
      plotdf["upper"] <- rep(quantiles[2, ], length(x))
      plotdf$time <- x
    }
    
    
  } else {
    pop_size <- .readOutputFile(population_size_log, burnin = burnin)
    times <- .readOutputFile(interval_change_points_log, burnin = burnin)
    
    # remove the last time because we are not going to use it
    # if the vector of times has the same number of elements
    # as the vector of population sizes
    if ( length(pop_size[[1]]) == length(times[[1]]) ) {
      cat("Removing last time ...\n")
      times_pruned <- list()
      for (i in seq_along(times)){
        res <- times[[i]][-length(times[[i]])]
        times_pruned[[i]] <- res
      }
      times <- times_pruned
    }
    
    orders <- lapply(times, order)
    
    pop_size_ordered <- list()
    for (i in seq_along(pop_size)){
      res <- c(pop_size[[i]][1], pop_size[[i]][-1][orders[[i]]])
      pop_size_ordered[[i]] <- res
    }
    
    pop_size_trajectories <- list()
    
    if (model == "constant"){
      
      for (i in seq_along(pop_size_ordered)) {
        if (length(times[[i]]) > 0) {
          f <- approxfun(sort(times[[i]]),
                         utils::tail(pop_size_ordered[[i]], n = -1),
                         yleft = pop_size_ordered[[i]][1],
                         yright = utils::tail(pop_size_ordered[[i]], n = 1),
                         method = "constant")
        } else {
          f <- function(t) pop_size_ordered[[i]][1] + t*0
        }
        pop_size_trajectories[[i]] <- f
      }
    } else if (model == "linear") {
      for (i in seq_along(pop_size_ordered)){
        f <- approxfun(c(0, sort(times[[i]])),
                       pop_size_ordered[[i]],
                       #yleft = pop_size_ordered[[i]][1],
                       yright = utils::tail(pop_size_ordered[[i]], n = 1),
                       method = "linear")
        pop_size_trajectories[[i]] <- f
      }
    } else {
      stop('Please provide as demographic model either "constant" or "linear".')
    }
    
    if ( is.null(min_age) ) {
      min_age <- min( unlist(times) )
      min_age <- max(min_age, 1E-2)
    }
    if ( is.null(max_age) ) {
      max_age <- max( unlist(times) )
    }
    
    if (spacing == "exponential"){
      x <- exp(seq(log(min_age), log(max_age), length.out = num_grid_points))
    } else if (spacing == "equal"){
      x <- seq(min_age, max_age, length.out = num_grid_points)
    } else {
      stop('Please provide as grid point spacing either "exponential" or "equal".')
    }
    
    m <- sapply(pop_size_trajectories, function(e) e(x))
    
    if (distribution == FALSE){
      quantiles <- apply(m, 1, function(x) quantile(x, probs = probs))
      
      plotdf <- dplyr::as_tibble(apply(m, 1, summary))
      plotdf$lower <- quantiles[1,]
      plotdf$upper <- quantiles[2,]
      plotdf$time <- x
    }
  }
  
  if (distribution == TRUE) {
    rownames(m) = x
    return(m)
  } else {
    return(plotdf)
  }
}

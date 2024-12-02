#' Process Diversification Rates
#'
#'
#' Processing the output of a episodic diversification rate analysis with
#' mass-extinction events.
#'
#' For processing the output of an episodic diversification rate analysis.
#' processDivRates() assumes that the epochs are fixed rather than inferred.
#' Additionally, it assumes that times correspond to rates such that the first
#' rate parameter (i.e. speciation[1]) corresponds to the present. Conversely,
#' the first time parameter (i.e. interval_times[1]) corresponds to the first
#' time interval after the present, moving backwards in time. processDivRates()
#' relies on readTrace and produces a list object that can be read by
#' plotDivRates() to visualize the results. For now, only one log file per
#' parameter type is accepted (i.e. log files from multiple runs must be
#' combined before reading into the function).
#'
#' @param speciation_time_log (vector of character strings or
#' single character string; "") Path to speciation times log file(s)
#' @param speciation_rate_log (vector of character strings or
#' single character string; "") Path to speciation rates log file(s)
#' @param extinction_time_log (vector of character strings or
#' single character string; "") Path to extinction times log file(s)
#' @param extinction_rate_log (vector of character strings or
#' single character string; "") Path to extinction rates log file(s)
#' @param fossilization_time_log (vector of character strings or
#' single character string; "") Path to fossilization times log file(s)
#' @param fossilization_rate_log (vector of character strings or
#' single character string; "") Path to fossilization rates log file(s)
#' @param burnin (single numeric value; default = 0) Fraction of generations to
#'  discard (if value provided is between 0 and 1) or number of generations (if
#'  value provided is greater than 1). Passed to readTrace().
#' @param probs (numeric vector; c(0.025, 0.975)) a vector of length two
#' containing the upper and lower bounds for the confidence intervals.
#' @param summary typically "mean" or "median"; the metric to summarize the
#' posterior distribution. Defaults to "median"
#' @return List object with processed rate and time parameters.
#'
#' @examples
#'
#' \donttest{
#' # download the example datasets to working directory
#'
#' url_ex_times <-
#'    "https://revbayes.github.io/tutorials/intro/data/primates_EBD_extinction_times.log"
#' dest_path_ex_times <- "primates_EBD_extinction_times.log"
#' download.file(url_ex_times, dest_path_ex_times)
#'
#' url_ex_rates <-
#'    "https://revbayes.github.io/tutorials/intro/data/primates_EBD_extinction_rates.log"
#' dest_path_ex_rates <- "primates_EBD_extinction_rates.log"
#' download.file(url_ex_rates, dest_path_ex_rates)
#'
#' url_sp_times <-
#'    "https://revbayes.github.io/tutorials/intro/data/primates_EBD_speciation_times.log"
#' dest_path_sp_times <- "primates_EBD_speciation_times.log"
#' download.file(url_sp_times, dest_path_sp_times)
#'
#' url_sp_rates <-
#'    "https://revbayes.github.io/tutorials/intro/data/primates_EBD_speciation_rates.log"
#' dest_path_sp_rates <- "primates_EBD_speciation_rates.log"
#' download.file(url_sp_rates, dest_path_sp_rates)
#'
#' # to run on your own data, change this to the path to your data file
#' speciation_time_file <- dest_path_sp_times
#' speciation_rate_file <- dest_path_sp_rates
#' extinction_time_file <- dest_path_ex_times
#' extinction_rate_file <- dest_path_ex_rates
#'
#' rates <- processDivRates(speciation_time_log = speciation_time_file,
#'                          speciation_rate_log = speciation_rate_file,
#'                          extinction_time_log = extinction_time_file,
#'                          extinction_rate_log = extinction_rate_file,
#'                          burnin = 0.25)
#'
#' # remove files
#' # WARNING: only run for example dataset!
#' # otherwise you might delete your data!
#' file.remove(dest_path_sp_times, dest_path_ex_times,
#'             dest_path_sp_rates, dest_path_ex_rates)
#' }
#'
#' @export

processDivRates <- function(speciation_time_log = "",
                            speciation_rate_log = "",
                            extinction_time_log = "",
                            extinction_rate_log = "",
                            fossilization_time_log = "",
                            fossilization_rate_log = "",
                            burnin = 0.25,
                            probs = c(0.025, 0.975),
                            summary = "median",
                            timeline = NULL
                            ) {
  # enforce argument matching
  if (is.character(speciation_time_log) == FALSE)
    stop("speciation_time_log must be a string")
  if (is.character(speciation_rate_log) == FALSE)
    stop("speciation_rate_log must be a string")
  if (is.character(extinction_time_log) == FALSE)
    stop("extinction_time_log must be a string")
  if (is.character(extinction_rate_log) == FALSE)
    stop("extinction_rate_log must be a string")
  if (is.character(fossilization_time_log) == FALSE)
    stop("fossilization_time_log must be a string")
  if (is.character(fossilization_rate_log) == FALSE)
    stop("fossilization_rate_log must be a string")
  
  # check that both fozzilization files are either provided or not provided
  if (fossilization_time_log != "" &
      fossilization_rate_log == "")
    stop("both please provide both fossilization rates and times, or neither")
  if (fossilization_time_log == "" &
      fossilization_rate_log != "")
    stop("both please provide both fossilization rates and times, or neither")
  
  # check if speciation times log file(s) exist
  do_speciation_time_log_exist <- file.exists(speciation_time_log)
  if (any(!do_speciation_time_log_exist)) {
    # print out paths to files that don't exist
    stop(
      paste0(
        "Some speciation_time_log files do not exist:",
        paste0("\t", speciation_time_log[!do_speciation_time_log_exist]),
        sep = "\n"
      )
    )
  }
  
  # check if speciation rates log file(s) exist
  do_speciation_rate_log_exist <- file.exists(speciation_rate_log)
  if (any(!do_speciation_rate_log_exist)) {
    # print out paths to files that don't exist
    stop(
      paste0(
        "Some speciation_rate_log files do not exist:",
        paste0("\t", speciation_rate_log[!do_speciation_rate_log_exist]),
        sep = "\n"
      )
    )
  }
  
  # check if extinction times log file(s) exist
  do_extinction_time_log_exist <- file.exists(extinction_time_log)
  if (any(!do_extinction_time_log_exist)) {
    # print out paths to files that don't exist
    stop(
      paste0(
        "Some extinction_time_log files do not exist:",
        paste0("\t", extinction_time_log[!do_extinction_time_log_exist]),
        sep = "\n"
      )
    )
  }
  
  # check if extinction rates log file(s) exist
  do_extinction_rate_log_exist <- file.exists(extinction_rate_log)
  if (any(!do_extinction_rate_log_exist)) {
    # print out paths to files that don't exist
    stop(
      paste0(
        "Some extinction_rate_log files do not exist:",
        paste0("\t", extinction_rate_log[!do_extinction_rate_log_exist]),
        sep = "\n"
      )
    )
  }
  
  # check if fossilization times log file(s) exist if provided
  if (fossilization_time_log != "") {
    do_fossilization_time_log_exist <-
      file.exists(fossilization_time_log)
    if (any(!do_fossilization_time_log_exist)) {
      # print out paths to files that don't exist
      stop(
        paste0(
          "Some fossilization_time_log files do not exist:",
          paste0("\t",
                 fossilization_time_log[!do_fossilization_time_log_exist]),
          sep = "\n"
        )
      )
    }
  }
  
  # check if fossilization rates log file(s) exist if provided
  if (fossilization_rate_log != "") {
    do_fossilization_rate_log_exist <- file.exists(fossilization_rate_log)
    if (any(!do_fossilization_rate_log_exist)) {
      # print out paths to files that don't exist
      stop(
        paste0(
          "Some fossilization_rate_log files do not exist:",
          paste0("\t",
                 fossilization_rate_log[!do_fossilization_rate_log_exist]),
          sep = "\n"
        )
      )
    }
  }

       
  # timeline 
  if (is.null(timeline)){
    times <- RevGadgets:::.readOutputFile(speciation_time_log, burnin = burnin)
    oldest_age <- max(sapply(times,max))
    timeline <- seq(0.0, oldest_age, length.out = 100)
  }
  
  z_lambda <- evaluate_timevarying_ratefunction(speciation_time_log, speciation_rate_log, timeline, burnin)
  z_mu     <- evaluate_timevarying_ratefunction(extinction_time_log, extinction_rate_log, timeline, burnin)
  
  if (fossilization_time_log == ""){
    z_psi   <- NULL
    df_fossilization <- tibble::tibble()
  } else {
    z_psi   <- evaluate_timevarying_ratefunction(fossilization_time_log, fossilization_rate_log, timeline, burnin)
    df_fossilization <- make_summary(z_psi, "fossilization rate", timeline, summary, probs)
  }
  
  ## re-parameterizations 
  z_relext <- z_mu / z_lambda 
  z_netdiv <- z_lambda - z_mu
  
  
  df_speciation <- make_summary(z_lambda, "speciation rate", timeline, summary, probs)
  df_mu <- make_summary(z_mu, "extinction rate", timeline, summary, probs)
  df_relext <- make_summary(z_relext, "relative-extinction rate", timeline, summary, probs)
  df_netdiv <- make_summary(z_netdiv, "net-diversification rate", timeline, summary, probs)
  
  
  plotdata <- dplyr::bind_rows(df_speciation, df_mu, df_relext, df_netdiv, df_fossilization)
  
  return(plotdata)
}

evaluate_timevarying_ratefunction <- function(fpath_times, fpath_rates, timeline, burnin){
  rates <- RevGadgets:::.readOutputFile(fpath_rates, burnin = burnin)
  times <- RevGadgets:::.readOutputFile(fpath_times, burnin = burnin)
  
  ## timevarying functions
  foos <- list()
  for (i in seq_along(rates)){
    if (length(rates[[i]]) > 1){
      f <- approxfun(times[[i]],
                     utils::tail(rates[[i]], n = -1),
                     yleft = rates[[i]][1],
                     yright = utils::tail(rates[[i]], n = 1),
                     method = "constant") ## assume piecewise-constant
    }else{
      f <- function(x) rates[[i]][1] + 0*x  #0*x is necessary, otherwise the function does not vectorize well
    }
    foos[[i]] <- f
  }
  
  ## evaluate all functions (per iteration) on the timeline
  ## z is a two-dimensional array with 
  ## - rows: corresponding to the points on the time axis (according to timeline)
  ## - columns: corresponding to the samples from the posterior distribution
  z <- sapply(foos, function(f) f(timeline))
  
  return(z)
}

make_summary <- function(z, item, timeline, summary, probs){
  if (!(summary %in% c("median", "mean"))){
    stop("summary must be either median or mean")
  }
  middle <- apply(z, 1, summary)
  uncertainty <- apply(z, 1, function(x) quantile(x, probs = probs))
  
  df <- tibble::tibble(
    "time" = timeline,
    "value" = middle,
    "lower" = uncertainty[1,],
    "upper" = uncertainty[2,],
    "item" = item,
  )
  return(df)
}
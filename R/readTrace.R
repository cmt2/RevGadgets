# Load necessary libraries
library(tidyverse)
library(jsonlite)
library(dplyr)

# Source the read_and_parse_json.R file to load readAndParseJSON function
source("R/read_parse_json.R")

#' Read trace
#'
#' Reads in MCMC log files or JSON files
#'
#' Reads in one or multiple MCMC log files from the same analysis
#' and discards a user-specified burn-in, compatible with multiple monitor
#' types. If the trace contains vectors of vectors and the user does not specify
#' format = "complex", readTrace() will read in those columns as factors
#' rather than as numeric vectors.
#'
#' @param paths (vector of character strings; no default) File path(s) to trace
#' file.
#' @param format (single character string; default = "simple") Indicates type of
#' MCMC trace, complex indicates cases where trace contains vectors of vectors/
#' matrices - mnStochasticVariable monitor will sometimes be of this type.
#' @param delim (single character string; default = "\t") Delimiter of file.
#' @param burnin (single numeric value; default = 0.1) Fraction of generations
#' to discard (if value provided is between 0 and 1) or number of generations
#' (if value provided is greater than 1).
#' @param check.names (logical; default = FALSE) Passed to utils::read.table();
#' indicates if utils::read.table() should check column names and replace
#' syntactically invalid characters.
#' @param ... (various) Additional arguments passed to utils::read.table().
#'
#' @return List of dataframes .
#'
#' 
#'  \donttest{
#' # How to call the function
#' output <- readTrace(paths = c("simplerev/simple/part_run_1.log", "simplerev/simple/part_run_2.log"),
#'                     format = "json",
#'                     delim = "\t",
#'                     burnin = 0.1,
#'                     check.names = FALSE)
#' 
#' # Display formatted output using a loop
#' for (i in seq_along(output)) {
#'   cat(paste("File", i, "\n"))
#'   print(output[[i]], row.names = TRUE)
#'   cat("\n")
#' }
#' # Example usage:
file <- readline(prompt= "Enter file path to JSONfile  :")
parsed_df <- readAndParseJSON(file)
# View the parsed and unnested data frame
View(parsed_df)





# Function to read trace files
readTrace <- function(paths,
                      format = "simple",
                      delim = "\t",
                      burnin = 0.1, 
                      check.names = FALSE, ...)
{
  
  # Enforce argument matching
  if (!is.character(paths)) {
    stop("All paths must be character strings.")
  }
  
  # Check if files exist
  if (!all(file.exists(paths))) {
    missing_files <- paths[!file.exists(paths)]
    stop("The following files do not exist:\n", paste(missing_files, collapse = "\n"))
  }
  
  # Ensure format is either "simple" or "complex" or "json"
  format <- match.arg(format, choices = c("simple", "complex", "json"))
  if (!is.character(delim) || nchar(delim) != 1) {
    stop("Delimiter must be a single character string.")
  }
  if (!is.numeric(burnin) || length(burnin) != 1 || burnin < 0) {
    stop("Burnin must be a single positive numeric value.")
  }
  
  # Helper function to read data based on file extension
  read_data <- function(path,format, delim, check.names, ...) {
    if (format == "json") {
      return(readAndParseJSON(path))
    } else {
      return(utils::read.table(
        file = path,
        header = TRUE,
        sep = delim,
        check.names = check.names,
        ...
      ))
    }
  }
  
  # Check that the file headings match for all traces
  headers <- lapply(paths, function(path) {
    data <- read_data(path,format, delim, check.names, nrows = 0, ...)
    colnames(data)
  })
  unique_headers <- unique(headers)
  if (length(unique_headers) > 1) {
    stop("Not all headers of trace files match.")
  }
  
  # Read in the traces
  output <- lapply(paths, function(path) {
    message(paste0("Reading log file: ", path))
    data <- read_data(path, format,  delim, check.names, ...)
    
    # Apply burnin if specified
    if (burnin >= nrow(data)) {
      stop("Burnin larger than provided trace file.")
    }
    if (burnin >= 1) {
      data <- data[(burnin + 1):nrow(data), ]
    } else if (burnin > 0) {
      discard <- ceiling(burnin * nrow(data))
      data <- data[(discard + 1):nrow(data), ]
    }
    
    return(data)
  })
  
  # Return each data frame separately (if multiple paths provided)
  if (length(output) > 1) {
    return(output)
  } else {
    return(output[[1]])
  }
}


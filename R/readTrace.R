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
#' @return List of dataframes (of length 1 if only 1 log file provided).
#'
#' @export
readTrace <- function(paths,
                      format = "simple",
                      delim = "\t",
                      burnin = 0.1,
                      check.names = FALSE,
                      ...) {
  # Enforce argument matching
  if (!is.character(paths)) {
    stop("All paths must be character strings.")
  }
  # Check if files exist
  if (!all(file.exists(paths))) {
    missing_files <- paths[!file.exists(paths)]
    stop("The following files do not exist:\n", paste(missing_files, collapse = "\n"))
  }
  # Ensure format is either "simple" or "complex"
  format <- match.arg(format, choices = c("simple", "complex"))
  if (!is.character(delim) || nchar(delim) != 1) {
    stop("Delimiter must be a single character string.")
  }
  if (!is.numeric(burnin) || length(burnin) != 1 || burnin < 0) {
    stop("Burnin must be a single positive numeric value.")
  }
  # Helper function to read data based on file extension
  read_data <- function(path, delim, check.names, ...) {
    ext <- tools::file_ext(path)
    if (ext == "json") {
      # Read JSON lines file directly into a list
      json_data <- stream_in(file(path), simplifyVector = FALSE)
      # Initialize an empty dataframe with headers
      headers <- c("Iteration", "Likelihood", "Posterior", "Prior",
                   "alpha_morpho", "alpha_morpho2", "br_len_lambda",
                   "rates_morpho1", "rates_morpho2", "tree_length")
      data <- matrix(NA, nrow = length(headers), ncol = length(json_data))
      # Extract data from each JSON object
      for (i in seq_along(json_data)) {
        obj <- json_data[[i]]
        data[, i] <- c(obj$Iteration, obj$Likelihood, obj$Posterior, obj$Prior,
                       obj$alpha_morpho, obj$alpha_morpho2, obj$br_len_lambda,
                       obj$rates_morpho[1], obj$rates_morpho2[1], obj$tree_length)
      }
      # Convert to dataframe and transpose
      data <- as.data.frame(data)
      colnames(data) <- NULL
      rownames(data) <- headers
      return(data)
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
    data <- read_data(path, delim, check.names, nrows = 0, ...)
    colnames(data)
  })
  unique_headers <- unique(headers)
  if (length(unique_headers) > 1) {
    stop("Not all headers of trace files match.")
  }
  # Read in the traces
  output <- lapply(seq_along(paths), function(i) {
    message(paste0("Reading in log file ", i))
    out <- read_data(paths[i], delim, check.names, ...)
    if (burnin >= nrow(out)) {
      stop("Burnin larger than provided trace file.")
    }
    if (burnin >= 1) {
      out <- out[(burnin + 1):nrow(out), ]
    } else if (burnin > 0) {
      discard <- ceiling(burnin * nrow(out))
      out <- out[(discard + 1):nrow(out), ]
    }
    return(out)
  })
  return(output)
}
# Load necessary libraries if not already loaded
library(tidyverse)
library(jsonlite)

# Function to read and parse JSON lines file
readAndParseJSON <- function(file) {
  
  # Function to safely parse each line of JSON
  parse_json_safe <- function(line) {
    tryCatch(
      fromJSON(line, flatten = TRUE),
      error = function(e) {
        message("Error parsing line: ", line)
        NULL
      }
    )
  }
  
  # Read JSON lines file line by line
  json_lines <- readLines(file)
  
  # Parse each line of JSON data
  parsed_data <- map(json_lines, parse_json_safe)
  
  # Filter out any NULL values that failed to parse
  parsed_data <- compact(parsed_data)
  
  # Combine all parsed JSON objects into a single data frame
  df <- bind_rows(parsed_data)
  
  # Unnest the list columns (customize as per your needs)
  df_long <- df %>%
    unnest(cols = c(rates_morpho, rates_morpho2), names_sep = "_")
  
  return(df_long)
}

# Example usage:
file <- "simplerev/simple/part_run_1.log"
parsed_df <- readAndParseJSON(file)

# View the parsed and unnested data frame
View(parsed_df)

#how to call the function
output <- readTrace(paths = c("simplerev/simple/part_run_1.log", "simplerev/simple/part_run_2.log"),
                    format = "simple",
                    delim = "\t",
                    burnin = 0.1,
                    check.names = FALSE)
# Display formatted output
for (i in seq_along(output)) {
  cat(paste("File", i, "\n"))
  print(output[[i]], row.names = TRUE)
  cat("\n")
}
print(output)
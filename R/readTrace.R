# Load necessary libraries
library(tidyverse)
library(jsonlite)
library(dplyr)

# Function to read and parse JSON lines file
readAndParseJSON <- function(file) {
  # Function to safely parse each line of JSON
  parse_json_safe <- function(line) {
    tryCatch(
      fromJSON(line, simplifyVector = FALSE),
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
  
  # Convert list columns to comma-separated strings
  parsed_data <- lapply(parsed_data, function(row) {
    row <- lapply(row, function(value) {
      if (is.list(value)) {
        paste(unlist(value), collapse = ",")
      } else {
        value
      }
    })
    return(row)
  })
  
  # Combine all parsed JSON objects into a single data frame
  df <- bind_rows(parsed_data)
  
  return(df)
}

# Function to read trace files
readTrace <- function(paths, format = "simple", delim = "\t", burnin = 0.1, check.names = FALSE, ...) {
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
    data <- read_data(path, delim, check.names, nrows = 0, ...)
    colnames(data)
  })
  unique_headers <- unique(headers)
  if (length(unique_headers) > 1) {
    stop("Not all headers of trace files match.")
  }
  
  # Read in the traces
  output <- lapply(paths, function(path) {
    message(paste0("Reading log file: ", path))
    data <- read_data(path, delim, check.names, ...)
    
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

# Example usage:
file <- "simplerev/simple/part_run_1.log"
parsed_df <- readAndParseJSON(file)

# View the parsed and unnested data frame
View(parsed_df)

# How to call the function
output <- readTrace(paths = c("simplerev/simple/part_run_1.log", "simplerev/simple/part_run_2.log"),
                    format = "simple",
                    delim = "\t",
                    burnin = 0.1,
                    check.names = FALSE)

# Display formatted output using a loop
for (i in seq_along(output)) {
  cat(paste("File", i, "\n"))
  print(output[[i]], row.names = TRUE)
  cat("\n")
}

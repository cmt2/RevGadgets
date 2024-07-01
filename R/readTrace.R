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
  message("Number of lines read: ", length(json_lines))
  
  # Function to check if a line is metadata (to skip very first line showing fields, formats, etc.)
  is_metadata_line <- function(line) {
    tryCatch({
      json <- fromJSON(line, simplifyVector = TRUE)
      # Check if the line contains specific metadata keys to skip
      any(names(json) %in% c("atomic", "fields", "format"))
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # Skip metadata lines
  parsed_data <- list()
  for (line in json_lines) {
    if (!is_metadata_line(line)) {
      parsed_data <- c(parsed_data, list(parse_json_safe(line)))
    }
  }
  
  # Filter out any NULL values that failed to parse
  parsed_data <- compact(parsed_data)
  message("Number of lines parsed successfully: ", length(parsed_data))
  
  # Convert list columns to separate columns
  parsed_data <- map(parsed_data, function(row) {
    flat_row <- list()
    for (name in names(row)) {
      if (is.list(row[[name]])) {
        values <- unlist(row[[name]])
        for (i in seq_along(values)) {
          flat_row[[paste(name, i, sep = "_")]] <- values[[i]]
        }
      } else {
        flat_row[[name]] <- row[[name]]
      }
    }
    return(flat_row)
  })
  
  # Combine all parsed JSON objects into a single data frame
  df <- bind_rows(parsed_data)
  message("Data frame created with ", nrow(df), " rows and ", ncol(df), " columns")
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

# Example usage
file <- "simple/part_run_1.log"
parsed_df <- readAndParseJSON(file)
# View the parsed and unnested data frame
View(parsed_df)

# How to call the function
output <- readTrace(paths = c("simple/part_run_1.log", "simple/part_run_2.log"),
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

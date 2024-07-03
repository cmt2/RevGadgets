# Load necessary libraries
library(tidyverse)

# Load your JSON parsing function from the separate file
source("R/json_utils.R")

# Example usage
file <- "simple/part_run_1.log"
parsed_df <- readAndParseJSON(file)
# View the parsed and unnested data frame
View(parsed_df)


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
      return(readAndParseJSON(path))  # Call the function from json_utils.R
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




# Load necessary libraries for readAndParseJSON
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
  # Initialize an empty list to store parsed data
  parsed_data <- list()
  
  # Function to check if a line is metadata
  is_metadata_line <- function(line) {
    tryCatch({
      json <- fromJSON(line, simplifyVector = TRUE)
      any(names(json) %in% c("atomic", "fields", "format"))
    }, error = function(e) {
      return(FALSE)
    })
  }
  
  # Skip metadata lines
  for (line in json_lines) {
    if (!is_metadata_line(line)) {
      parsed_data <- c(parsed_data, list(parse_json_safe(line)))
    }
  }
  
  # Filter out any NULL values that failed to parse
  parsed_data <- compact(parsed_data)
  
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

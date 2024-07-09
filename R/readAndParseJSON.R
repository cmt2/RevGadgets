#' Read and Parse JSON lines file
#' Internal function to read in JSON files
#'
#' @param file (character) File path to trace file.
#' @return Data frame.
#' @export
readAndParseJSON <- function(file) {
  # Function to safely parse each line of JSON
  parse_json_safe <- function(line) {
    tryCatch(
      jsonlite::fromJSON(line, simplifyVector = FALSE),
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
      json <- jsonlite::fromJSON(line, simplifyVector = TRUE)
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
  parsed_data <- purrr::compact(parsed_data)
  
  # Convert list columns to separate columns
  parsed_data <- purrr::map(parsed_data, function(row) {
    flat_row <- list()
    for (name in names(row)) {
      if (is.list(row[[name]])) {
        # Flatten lists into individual columns
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
  df <- dplyr::bind_rows(parsed_data)
  message("Data frame created with ", nrow(df), " rows and ", ncol(df), " columns")
  
  return(df)
}

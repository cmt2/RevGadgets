#' Read trace
#'
#' Reads in MCMC log files and processes them according to specified parameters.
#'
#' @param paths (character vector) File path(s) to trace file(s).
#' @param format (character) Indicates type of MCMC trace. Options: "simple", "complex", "json".
#' @param delim (character) Delimiter of file (default: "\t").
#' @param burnin (numeric) Fraction of generations to discard (if < 1) or number of generations (if >= 1) (default: 0.1).
#' @param check.names (logical) Indicates if column names should be checked and replaced (default: FALSE).
#' @param verbose (logical) Whether to display progress messages (default: TRUE).
#' @param ... (additional arguments) Passed to read.table() for file reading.
#'
#' @return List of data frames (of length 1 if only 1 log file provided).
#'
#' @examples
#' \dontrun{
#' # Example usage:
#' file <- "../simple/part_run_1.log"
#' parsed_df <- readTrace(file, format = "simple")
#' View(parsed_df)  # View the parsed data frame
#' }
#'
#' @export
readTrace <- function(paths,
                      format = "simple",
                      delim = "\t",
                      burnin = 0.1,
                      check.names = FALSE,
                      verbose = TRUE,
                      ...) {
  
  # Ensure paths are character strings
  if (!is.character(paths)) {
    stop("All paths must be character strings.")
  }
  
  # Check if files exist
  if (!all(file.exists(paths))) {
    missing_files <- paths[!file.exists(paths)]
    stop("The following files do not exist:\n", paste(missing_files, collapse = "\n"))
  }
  
  # Ensure format is valid
  format <- match.arg(format, choices = c("simple", "complex", "json"))
  
  # Validate delim
  if (!is.character(delim) || nchar(delim) != 1) {
    stop("delim must be a single character string")
  }
  
  # Validate burnin
  if (!is.numeric(burnin) || burnin < 0) {
    stop("burnin must be a single non-negative numeric value")
  }
  
  num_paths <- length(paths)
  
  # Check that the file headings match for all traces
  header <- vector("list", num_paths)
  for (i in seq_len(num_paths)) {
    if (format == "json") {
      # Use your custom function to read and parse JSON
      json_data <- readAndParseJSON(paths[i])
      header[[i]] <- names(json_data)
    } else {
      header[[i]] <- colnames(
        utils::read.table(
          file = paths[i],
          header = TRUE,
          sep = delim,
          check.names = check.names,
          nrows = 1,  # Read only the header row to check column names
          ...
        )
      )
    }
  }
  
  # Ensure all headers are identical
  all_headers <- unique(unlist(header))
  for (i in seq_len(num_paths)) {
    if (!identical(all_headers, header[[i]])) {
      stop("Not all headers of trace files match")
    }
  }
  
  # Read in the traces
  output <- vector("list", num_paths)
  for (i in seq_len(num_paths)) {
    if (verbose) {
      message(paste0("Reading in log file ", i, "\n"))
    }
    
    if (format == "json") {
      # Use your custom function to read and parse JSON
      out <- readAndParseJSON(paths[i])
      out <- as.data.frame(out)  # Convert JSON data to data frame if necessary
    } else if (format == "complex") {
      stop("Complex trace type currently not supported")
    } else {
      out <- utils::read.table(
        file = paths[i],
        header = TRUE,
        sep = delim,
        check.names = check.names,
        ...
      )
    }
    
    # Discard burn-in generations
    if (burnin >= nrow(out)) {
      stop("Burnin larger than provided trace file")
    }
    
    if (burnin >= 1) {
      output[[i]] <- out[(burnin + 1):nrow(out), ]
    } else if (burnin < 1 & burnin > 0) {
      discard <- ceiling(burnin * nrow(out))
      output[[i]] <- out[(discard + 1):nrow(out), ]
    } else if (burnin == 0) {
      output[[i]] <- out
    } else {
      stop("Invalid burnin value")
    }
  }
  
  # Combine the data frames if there are multiple files
  if (num_paths > 1) {
    output <- do.call(rbind, output)
  } else {
    output <- output[[1]]
  }
  
  # Return list of data frames
  return(output)
}

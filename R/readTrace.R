#' Read trace
#'
#' Reads in MCMC log files
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
#' matrices - mnStochasticVariable monitor will sometimes be of this type. When
#' `format = "json"`, the log will be parsed as a JSON file.
#' matrices - mnStochasticVariable monitor will sometimes be of this type.
#' @param delim (single character string; default = "\t") Delimiter of file.
#' @param burnin (single numeric value; default = 0.1) Fraction of generations
#' to  discard (if value provided is between 0 and 1) or number of generations
#' (if value provided is greater than 1).
#' @param check.names (logical; default = FALSE) Passed to utils::read.table();
#' indicates if utils::read.table() should check column names and replace
#' syntactically invalid characters.
#' @param verbose (logical; default = TRUE) Print status of reading traces 
#' to screen
#' @param ... (various) Additional arguments passed to utils::read.table().
#'
#' @return List of dataframes (of length 1 if only 1 log file provided).
#'
#' @examples
#' # read and process a single trace file
#' \donttest{
#' # Create dummy data
#' temp_file1 <- tempfile(fileext = ".log")
#' temp_file2 <- tempfile(fileext = ".log")
#' write.table(data.frame(a = 1:10, b = 11:20), file = temp_file1, sep = "\t", row.names = FALSE)
#' write.table(data.frame(a = 21:30, b = 31:40), file = temp_file2, sep = "\t", row.names = FALSE)
#'
#' # How to call the function
#' output <- readTrace(paths = c(temp_file1, temp_file2),
#'                     format = "simple",
#'                     delim = "\t",
#'                     burnin = 0.1,
#'                     check.names = FALSE)
#'
#' # Clean up temporary files
#' file.remove(temp_file1, temp_file2)
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
  # enforce argument matching
  
  character_paths_are_strings <- is.character(paths)
  if (any(character_paths_are_strings == FALSE) == TRUE) {
    # print out the ones that are not character strings
    stop(
      paste0("Some paths are not character strings:",
             paste0("\t", paths[character_paths_are_strings == FALSE]),
             sep = "\n")
    )
  }
  
  do_files_exist <- file.exists(paths)
  if (any(do_files_exist == FALSE) == TRUE) {
    # print out paths to files that don't exist
    stop(
      paste0("Some files do not exist:",
             paste0("\t", paths[do_files_exist == FALSE]), sep = "\n")
    )
  }
  
  format <- match.arg(format, choices = c("simple", "complex", "json"))
  
  if (is.character(delim) == FALSE)
    stop("delim must be a single character string")
  
  if (is.numeric(burnin) == FALSE)
    stop("burnin must be a single numeric value")
  if (burnin < 0)
    stop("burnin must be a positive value")
  
  num_paths <- length(paths)
  
  # check that the file headings match for all traces
  header <- vector("list", num_paths)
  for (i in 1:num_paths) {
    header[[i]] <- colnames(
      utils::read.table(
        file = paths[i],
        header = TRUE,
        sep = delim,
        check.names = check.names,
        nrows = 0,
        ...
      )
    )
  }
  
  all_headers <- unique(unlist(header))
  for (i in seq_len(length(header))) {
    if (setequal(all_headers, header[[i]]) == FALSE) {
      stop("Not all headers of trace files match")
    }
  }
  
  # read in the traces
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
  
  # Return list of data frames
  return(output)
}

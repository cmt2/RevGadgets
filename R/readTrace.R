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
#' @param format (single character string; default = simple) Indicates type of
#' MCMC trace, complex indicates cases where trace contains vectors of vectors/

#' matrices - mnStochasticVariable monitor will sometimes be of this type. When
#' `format = "json"`, the log will be parsed as a JSON file.
#' matrices - mnStochasticVariable monitor will sometimes be of this type.

#' @param delim (single character string; default = "\\t") Delimiter of file.
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
#'
#' \donttest{

#' # How to call the function
#' output <- readTrace(paths = c("simplerev/simple/part_run_1.log", "simplerev/simple/part_run_2.log"),
#'                     format = "json",
#'                     delim = "\\t",
#'                     burnin = 0.1,
#'                     check.names = FALSE)

#' # download the example dataset to working directory
#' url_gtr <-
#'    "https://revbayes.github.io/tutorials/intro/data/primates_cytb_GTR.log"
#' dest_path_gtr <- "primates_cytb_GTR.log"
#' download.file(url_gtr, dest_path_gtr)

#'
#' # to run on your own data, change this to the path to your data file
#' file_single <- dest_path_gtr
#'
#' one_trace <- readTrace(paths = file_single)
#'
#' # remove file
#' # WARNING: only run for example dataset!
#' # otherwise you might delete your data!
#' file.remove(dest_path_gtr)
#'
#' # read and process multiple trace files, such as from multiple runs of
#' # the same analysis
#'
#' # download the example dataset to working directory
#' url_1 <-
#' "https://revbayes.github.io/tutorials/intro/data/primates_cytb_GTR_run_1.log"
#' dest_path_1 <- "primates_cytb_GTR_run_1.log"
#' download.file(url_1, dest_path_1)
#'
#' url_2 <-
#' "https://revbayes.github.io/tutorials/intro/data/primates_cytb_GTR_run_2.log"
#' dest_path_2 <- "primates_cytb_GTR_run_2.log"
#' download.file(url_2, dest_path_2)
#'
#' # to run on your own data, change this to the path to your data file
#' file_1 <- dest_path_1
#' file_2 <- dest_path_2
#'
#' # read in the multiple trace files
#' multi_trace <- readTrace(path = c(file_1, file_2), burnin = 0.0)
#'
#' # remove files
#' # WARNING: only run for example dataset!
#' # otherwise you might delete your data!
#' file.remove(dest_path_1, dest_path_2)
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

  format <- match.arg(format, choices = c("simple", "complex"))

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
  if (format == "simple") {
    for (i in 1:num_paths) {
      if (verbose) {
        message(paste0(paste0("Reading in log file ", i), "\n", sep = ""))
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

      if (burnin >= nrow(out))
        stop("Burnin larger than provided trace file")

      if (burnin >= 1) {
        output[[i]] <- out[(burnin + 1):nrow(out),]
      } else if (burnin < 1 & burnin > 0) {
        discard <- ceiling(burnin * nrow(out))
        output[[i]] <- out[(discard + 1):nrow(out),]
      } else if (burnin == 0) {
        output[[i]] <- out
      } else {
        stop("What have you done?")
      }
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
}
  
  # Return list of data frames

  } else if (format == "complex") {
    stop("Complex trace type currently not supported")
  } else {
    stop("Format is not of type simple or complex")
  }


  return(output)
}

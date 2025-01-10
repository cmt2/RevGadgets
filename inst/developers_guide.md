## Intro

The purpose of this vignette is to establish common practices developing the RevGadgets package. These practices include documenting, writing, and testing of functions, as well as appropriate use of unit testing through GitHub actions.

Please be sure to do all development using the development branch of RevGadgets or a feature branch (for example dev_pop_size). Adding new functionality should be done on a feature branch (branched from development), but minor fixes to existing code can happen directly on development. Once the feature branch is good to go, submit a pull request to merge back with development. We will occasionally merge the development and master branches for new releases. 

We will use the following two example functions to illustrate these best practices:

### Example 1: processDivRates()

This function processes the output of the episodic birth-death process tutorial from RevBayes, making the data available for subsequent plotting. It relies on the function `readTrace()`. If you are curious about that function, please see the corresponding documentation in the R package.
```R

#' Process Diversification Rates
#'
#'
#' Processing the output of a episodic diversification rate analysis with mass-extinction events.
#'
#' For processing the output of an episodic diversification rate analysis. processDivRates()
#' assumes that the epochs are fixed rather than inferred. Additionally, it assumes
#' that times correspond to rates such that the first rate parameter (i.e. speciation[1])
#' corresponds to the present. Conversely, the first time parameter
#' (i.e. interval_times[1]) corresponds to the first time interval after the present,
#' moving backwards in time. processDivRates() relies on readTrace and produces a list
#' object that can be read by plotDivRates() to vizualize the results. For now,
#' only one log file per parameter type is accepted (i.e. log files from multiple runs
#' must be combined before reading into the function).
#'
#'@param speciation_time_log (vector of character strings or single character string; "") Path to speciation times log file(s)
#'@param speciation_rate_log (vector of character strings or single character string; "") Path to speciation rates log file(s)
#'@param extinction_time_log (vector of character strings or single character string; "") Path to extinction times log file(s)
#'@param extinction_rate_log (vector of character strings or single character string; "") Path to extinction rates log file(s)
#'@param burnin (single numeric value; default = 0) Fraction of generations to
#' discard (if value provided is between 0 and 1) or number of generations (if
#' value provided is greater than 1). Passed to readTrace().
#'
#'@return List object with processed rate and time parameters.
#'
#'@examples
#'
#'\dontrun{
#'
#' speciation_time_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_speciation_times.p", package="RevGadgets")
#' speciation_rate_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_speciation_rates.p", package="RevGadgets")
#' extinction_time_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_extinction_times.p", package="RevGadgets")
#' extinction_rate_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_extinction_rates.p", package="RevGadgets")
#'
#' primates <- processDivRates(speciation_time_log = speciation_time_file,
#'                             speciation_rate_log = speciation_rate_file,
#'                             extinction_time_log = extinction_time_file,
#'                             extinction_rate_log = extinction_rate_file,
#'                             burnin = 0.25)
#'}
#'
#' @export

processDivRates <- function(speciation_time_log = "",
                            speciation_rate_log = "",
                            extinction_time_log = "",
                            extinction_rate_log = "",
                            burnin = 0.25) {

  # enforce argument matching
  if (is.character(speciation_time_log) == FALSE) stop("speciation_time_log must be a single character string")
  if (is.character(speciation_rate_log) == FALSE) stop("speciation_rate_log must be a single character string")
  if (is.character(extinction_time_log) == FALSE) stop("extinction_time_log must be a single character string")
  if (is.character(extinction_rate_log) == FALSE) stop("extinction_rate_log must be a single character string")

  # check if speciation times log file(s) exist
  do_speciation_time_log_exist <- file.exists(speciation_time_log)
  if ( any(do_speciation_time_log_exist == FALSE) == TRUE ) {
    # print out paths to files that don't exist
    cat( "Some speciation_time_log files do not exist:",
         paste0("\t",speciation_time_log[do_speciation_time_log_exist == FALSE]), sep="\n")
    stop()
  }

  # check if speciation rates log file(s) exist
  do_speciation_rate_log_exist <- file.exists(speciation_rate_log)
  if ( any(do_speciation_rate_log_exist == FALSE) == TRUE ) {
    # print out paths to files that don't exist
    cat( "Some speciation_rate_log files do not exist:",
         paste0("\t",speciation_rate_log[do_speciation_rate_log_exist == FALSE]), sep="\n")
    stop()
  }

  # check if extinction times log file(s) exist
  do_extinction_time_log_exist <- file.exists(extinction_time_log)
  if ( any(do_extinction_time_log_exist == FALSE) == TRUE ) {
    # print out paths to files that don't exist
    cat( "Some extinction_time_log files do not exist:",
         paste0("\t",extinction_time_log[do_extinction_time_log_exist == FALSE]), sep="\n")
    stop()
  }

  # check if extinction rates log file(s) exist
  do_extinction_rate_log_exist <- file.exists(extinction_rate_log)
  if ( any(do_extinction_rate_log_exist == FALSE) == TRUE ) {
    # print out paths to files that don't exist
    cat( "Some extinction_rate_log files do not exist:",
         paste0("\t",extinction_rate_log[do_extinction_rate_log_exist == FALSE]), sep="\n")
    stop()
  }

  # read in log files as lists of data.frames with readTrace()
  speciation_time <- readTrace(path = speciation_time_log,
                                burnin = burnin)
  speciation_rate <- readTrace(path = speciation_rate_log,
                                burnin = burnin)
  extinction_time <- readTrace(path = extinction_time_log,
                                burnin = burnin)
  extinction_rate <- readTrace(path = extinction_rate_log,
                                burnin = burnin)

  # check if all parameter types have the same number of log files
  trace_lengths_same <- identical(length(speciation_time),
                                  length(speciation_rate),
                                  length(extinction_time),
                                  length(extinction_rate))

  if (trace_lengths_same == FALSE) {
    stop("You must provide the same number of log files for each parameter type.")}
  else if (trace_lengths_same == TRUE) {
    if (length(speciation_time) == 0) {
      stop("You must provide at least one log file per parameter type.")
    } else if (length(speciation_time) > 1) {
      stop("Currently, only one log file per parameter type is supported.")
    } else if (length(speciation_time) == 1) {

      # convert single item lists to data frames
      speciation_time <- speciation_time[[1]]
      speciation_rate <- speciation_rate[[1]]
      extinction_time <- extinction_time[[1]]
      extinction_rate <- extinction_rate[[1]]

      # add in dummy distribution of 0 for time in the present
      speciation_time$`interval_times[0]` <- rep(0, nrow(speciation_time))
      extinction_time$`interval_times[0]` <- rep(0, nrow(extinction_time))

      # Calculate the net-diversification and relative-extinction rates
      net_diversification_rate <- as.matrix(speciation_rate[,5:ncol(speciation_rate)]) -
                                   as.matrix(extinction_rate[,5:ncol(extinction_rate)])

      colnames(net_diversification_rate) <- paste(rep("net_div", times = ncol(net_diversification_rate)),
                                                   rep("[", times = ncol(net_diversification_rate)),
                                                   1:ncol(net_diversification_rate),
                                                   rep("]", times = ncol(net_diversification_rate)), sep = "")

      relative_extinction_rate <- as.matrix(extinction_rate[,5:ncol(extinction_rate)]) /
                                   as.matrix(speciation_rate[,5:ncol(speciation_rate)])

      colnames(relative_extinction_rate) <- paste(rep("rel_ext", times = ncol(relative_extinction_rate)),
                                                   rep("[", times = ncol(relative_extinction_rate)),
                                                   1:ncol(relative_extinction_rate),
                                                   rep("]", times = ncol(relative_extinction_rate)), sep = "")

        # return a list of processed data frames
        res <- list("speciation rate" = speciation_rate,
                    "extinction rate" = extinction_rate,
                    "net-diversification rate" = net_diversification_rate,
                    "relative-extinction rate" = relative_extinction_rate,
                    "speciation time" = speciation_time,
                    "extinction time" = extinction_time)
        return(res)
    }
  }
}


```

### Example 2: plotDivRates()

This function plots the results of the episodic birth-death process tutorial, taking as input the processed rates produced by `processDivRates()` above. We will mostly refer to this `plotDivRates()` function for this example.
```R
#' Plot Diversification Rates
#'
#' Plots the output of a episodic diversification rate analysis
#'
#' Plots the output of diversification rate analyses. Takes as input the
#' output of processDivRates() and plotting parameters. Does not return an
#' object. For now, valid figure types include: "speciation rate",
#' "extinction rate","net-diversification rate", and "relative-extinction rate".
#' If colors are not provided (parameter col), the plot defaults to preset colors.
#'
#'
#' @param output (list; no default) The processed output for plotting (output of processDivRates()).
#' @param fig_types (character vector, c("speciation rate", "extinction rate", "net-diversification rate", "relative-extinction rate")) Which aspects of the model to visualize. See details for a complete description.
#' @param xlab (character; "million years ago") The label of the x-axis.
#' @param col (character; NULL) Colors used for printing in hex code. Must be of same length as fig_types.
#' @param col_alpha (character; "50") Alpha channel parameter for credible intervals plotting. May range from 00 (completely transparent) to FF (completely opaque).
#' @param offset (numeric; 0) Value to offset the x-axis by.
#' @param xaxt (character; "n") The type of x-axis to plot. By default, no x-axis is plotted (recommended).
#' @param yaxt (character; "s") The type of y-axis to plot.
#' @param ... Parameters delegated to various plotting functions.
#'
#' @return Plots diversification rates, does not return an object.
#'
#' @examples
#'
#' \dontrun{
#' # first run processDivRates()
#' speciation_time_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_speciation_times.p", package="RevGadgets")
#' speciation_rate_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_speciation_rates.p", package="RevGadgets")
#' extinction_time_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_extinction_times.p", package="RevGadgets")
#' extinction_rate_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_extinction_rates.p", package="RevGadgets")
#'
#' primates <- processDivRates(speciation_time_log = speciation_time_file,
#'                             speciation_rate_log = speciation_rate_file,
#'                             extinction_time_log = extinction_time_file,
#'                             extinction_rate_log = extinction_rate_file,
#'                             burnin = 0.25)
#' # then plot results:
#' plotDivRates(output = primates)
#' }
#'
#' @export
#' @importFrom graphics plot polygon
#' @importFrom stats quantile

plotDivRates <- function(output,
                         fig_types = c("speciation rate",
                                       "extinction rate",
                                       "net-diversification rate",
                                       "relative-extinction rate"),
                         xlab = "million years ago",
                         col = NULL,
                         col_alpha = "50",
                         offset = 0,
                         xaxt = "n",
                         yaxt = "s",
                         ...){
  # Enforce argument matching
  if (is.list(output) == FALSE) stop("output must be list of processed rates")
  if (is.character(fig_types) == FALSE) stop("fig_types must be a character string or vector of strings")
  if (is.character(xlab) == FALSE) stop("xlab must be a single character string")

  # check length of color vector and ensure colors are formatted as hex codes
  if (is.null(col) == FALSE) {
    if (length(col) != length(fig_types))
      stop("col and fig_types must be of the same length")
    bad_cols <- rep("FALSE", times = length(col))
    for (i in 1:length(col)) {
      col_split <- unlist(strsplit(col[i], split = ""))
      if (length(col_split) != 7 | col_split[1] != "#") {
        bad_cols[i] <- TRUE
      }
    }
    if ( any(bad_cols == TRUE) == TRUE ) {
      # print out colors that are 'bad'
      cat( "Some colors are not properly formatted hex codes:",
           paste0("\t",col[bad_cols == TRUE]), sep="\n")
      stop()
    }
  }

  if (is.character(col_alpha) == FALSE) stop("col_alpha must be a character string")
  if (is.numeric(offset) == FALSE) stop("offset must be a numeric value")
  xaxt <- match.arg(xaxt, choices = c("n", "s"))
  yaxt <- match.arg(yaxt, choices = c("n", "s"))


  # Check that fig type is valid
  valid_fig_types <- c("speciation rate",
                       "extinction rate",
                       "net-diversification rate",
                       "relative-extinction rate")

  invalid_fig_types <- fig_types[!fig_types %in% valid_fig_types]

  if ( length(invalid_fig_types) > 0 ) {
    stop("\nThe following figure types are invalid: ", paste(invalid_fig_types, collapse = ", "), ".",
         "\nValid options are: ", paste(valid_fig_types, collapse = ", ") ,".")
  }


  # Make color vector
  if ( is.null(col) ) {
    col <- c("speciation rate"="#984EA3",
             "speciation shift times"="#984EA3",
             "speciation Bayes factors"="#984EA3",
             "extinction rate"="#E41A1C",
             "extinction shift times"="#E41A1C",
             "extinction Bayes factors"="#E41A1C",
             "fossilization rate"="#ffa31a",
             "fossilization shift times"="#ffa31a",
             "fossilization Bayes factors"="#ffa31a",
             "net-diversification rate"="#377EB8",
             "relative-extinction rate"="#FF7F00",
             "mass extinction times"="#4DAF4A",
             "mass extinction Bayes factors"="#4DAF4A")
  } else {
    names(col) <- fig_types
  }

  # Compute the axes
  intervals <- output$`speciation time`[1,grep("interval", colnames(output$`speciation time`))]
  tree_age <- max(intervals)
  num_intervals <- length(intervals) - 1
  plot_at <- 0:num_intervals
  interval_size <- tree_age/num_intervals
  labels <- pretty( c(0,tree_age) ) + offset
  labels_at <- num_intervals - ( pretty(c(0,tree_age)) / interval_size )
  ages <- seq( 0,tree_age, length.out = num_intervals + 1 )

  for( type in fig_types ) {

      this_output <- output[[type]][ ,grep("[0-9]", colnames(output[[type]])) ]
      mean_this_output <- colMeans(this_output)
      quantiles_this_output <- apply(this_output, 2, quantile, prob = c(0.025,0.975))

      # find the limits of the y-axis
      # we always want the speciation and extinction rates to be on the same scale
      if ( type %in% c("speciation rate","extinction rate")) {

        quantiles_speciation <-
          apply(output[["speciation rate"]][ ,grep("[0-9]", colnames(output[["speciation rate"]])) ],
                2, quantile, prob=c(0.025,0.975))
        quantiles_extinction <-
          apply(output[["extinction rate"]][ ,grep("[0-9]", colnames(output[["extinction rate"]])) ],
                2, quantile, prob=c(0.025,0.975))
        ylim <- c( min(0, quantiles_speciation, quantiles_extinction),
                   max(quantiles_speciation, quantiles_extinction) )

      } else {
        ylim <- c( min(0, quantiles_this_output),
                   max(quantiles_this_output) )
      }

      #### plot
        plot( x = plot_at, y = c( rev(mean_this_output)),
              type = "l", ylim = ylim, xaxt = xaxt, col = col[type],
              ylab = "rate", main = type, xlab = xlab,...)
        polygon( x = c( 0:ncol(quantiles_this_output),
                        ncol(quantiles_this_output):0 ),
                 y = c( rev(c(quantiles_this_output[1,1],
                          quantiles_this_output[1,])),
                        c(quantiles_this_output[2,1],
                              quantiles_this_output[2,])),
                 border = NA, col = paste(col[type], col_alpha, sep="") )
        axis(1, at = labels_at, labels = labels)
      }

    }
```

## General

Each function should be defined in an R script with the name of the function as the file name. For example the `plotDivRates()` function is defined in a source file called: `plotDivRates.R`. These files should be stored in the RevGadgets > R subdirectory. Functions that are called internally but are not exported should be named with a period proceeding the function name (for example: `.colFun()`), put in the `utils.R` script and kept in alphabetical order. There's no need to test or document these internal functions.

## Package Dependencies

Your functions may depend on objects or functions from other packages, such as `ape`, `ggtree`, etc. Please do not load in packages within functions. All packages that RevGadgets functions depend on should be included in the Imports line of the Description file, located in the base directory, which currently includes these packages:

```
Imports: ape (>= 5.4), phytools (>= 0.7-70), dplyr (>= 1.0.0),
         ggtree (>= 3.6.1), tidytree (>= 0.3.4), treeio (>= 1.12.0),
         ggplot2 (>= 3.4.0), reshape (>= 0.8.8), methods (>= 4.1.0),
         tidyr (>= 1.1.0), tibble (>= 3.0.1), gginnards (>= 0.0.3),
          ggplotify (>= 0.0.5), ggpp, png (>= 0.1-7), 
         stats (>= 4.0.1), utils (>= 4.0.1), grDevices (>= 4.0.1), 
         deeptime (>= 0.1.0), scales (>= 1.1.1)
```

By including dependency packages here, this ensures that the dependencies will be loaded when RevGadgets is loaded, and will be installed when RevGadgets is installed.

## Document Function

We will use the package `roxygen2` to document functions. Function documentation should occur at the very beginning of the source file. In `plotDivRates.R`, this documentation appears as:
```R
#' Plot Diversification Rates
#'
#' Plots the output of a episodic diversification rate analysis
#'
#' Plots the output of diversification rate analyses. Takes as input the
#' output of processDivRates() and plotting parameters. Does not return an
#' object. For now, valid figure types include: "speciation rate",
#' "extinction rate","net-diversification rate", and "relative-extinction rate".
#' If colors are not provided (parameter col), the plot defaults to preset colors.
#'
#'
#' @param output (list; no default) The processed output for plotting (output of processDivRates()).
#' @param fig_types (character vector, c("speciation rate", "extinction rate", "net-diversification rate", "relative-extinction rate")) Which aspects of the model to visualize. See details for a complete description.
#' @param xlab (character; "million years ago") The label of the x-axis.
#' @param col (character; NULL) Colors used for printing in hex code. Must be of same length as fig_types.
#' @param col_alpha (character; "50") Alpha channel parameter for credible intervals plotting. May range from 00 (completely transparent) to FF (completely opaque).
#' @param offset (numeric; 0) Value to offset the x-axis by.
#' @param xaxt (character; "n") The type of x-axis to plot. By default, no x-axis is plotted (recommended).
#' @param yaxt (character; "s") The type of y-axis to plot.
#' @param ... Parameters delegated to various plotting functions.
#'
#' @return Plots diversification rates, does not return an object.
#'
#' @examples
#'
#' \dontrun{
#' # first run processDivRates()
#' speciation_time_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_speciation_times.p", package="RevGadgets")
#' speciation_rate_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_speciation_rates.p", package="RevGadgets")
#' extinction_time_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_extinction_times.p", package="RevGadgets")
#' extinction_rate_file <- system.file("extdata",
#'     "epi_bd/primates_EBD_extinction_rates.p", package="RevGadgets")
#'
#' primates <- processDivRates(speciation_time_log = speciation_time_file,
#'                             speciation_rate_log = speciation_rate_file,
#'                             extinction_time_log = extinction_time_file,
#'                             extinction_rate_log = extinction_rate_file,
#'                             burnin = 0.25)
#' # then plot results:
#' plotDivRates(output = primates)
#' }
#'
#' @export
#' @importFrom graphics plot polygon
#' @importFrom stats quantile
```

Roxygen2 documentation begins with #' to distinguish it from other comments in the file. The first sentence becomes the function title and should be formatted as a sentence. The second paragraph is the description of the function and should briefly describe the purpose of the function. The third and any additional paragraphs correspond to the Details section of the function documentation, and should provide a more comprehensive description of the function.

The tag **@param** should be used to specify all parameters of the function, including information about the necessary format, default value (if exists), and a description of its purpose and/or possible values. For example, we have defined the path parameter as:
```R
#'@param output (list; no default) The processed output for plotting (output of processDivRates()).
```

Here, the **@param** tag indicates that we are describing a parameter, output corresponds to the exact parameter name, (list; no default) refers to the format of the parameter input and the (lack of) a default value, and "The processed output for plotting (output of processDivRates())." describes the parameter in text.

The tag **@return** describes the object that the function returns (in this case no value is returned, the function simply produces plots) and is included under the Value section of the documentation file.

The tag **@examples** provides the user with examples of the function in use. Often, it is most useful to provide multiple examples that encompass the variety of parameters and uses. Please note that we specify to not run examples during unit testing using `\dontrun{}`. This ensures that unit tests are run more efficiently. However, please be sure to test your examples to ensure the work. For more information on proper documentation of functions, refer to the `roxygen2` package information.

## Export and Import

We specify that we want the function to be exported as part of our package (available directly to the user as opposed to used internally) using @export, and we specify any functions used inside our function from external packages using the @import tag, followed by the package name and the names of the function(s).
```R
#' @export
#' @importFrom graphics plot polygon
#' @importFrom stats quantile
```

## Write Function

The following code defines our example function, which allows the user to plot the processed output of the episodic birth-death process tutorial.

```R
plotDivRates <- function(output,
                         fig_types = c("speciation rate",
                                       "extinction rate",
                                       "net-diversification rate",
                                       "relative-extinction rate"),
                         xlab = "million years ago",
                         col = NULL,
                         col_alpha = "50",
                         offset = 0,
                         xaxt = "n",
                         yaxt = "s",
                         ...){
  # Enforce argument matching
  if (is.list(output) == FALSE) stop("output must be list of processed rates")
  if (is.character(fig_types) == FALSE) stop("fig_types must be a character string or vector of strings")
  if (is.character(xlab) == FALSE) stop("xlab must be a single character string")

  # check length of color vector and ensure colors are formatted as hex codes
  if (is.null(col) == FALSE) {
    if (length(col) != length(fig_types))
      stop("col and fig_types must be of the same length")
    bad_cols <- rep("FALSE", times = length(col))
    for (i in 1:length(col)) {
      col_split <- unlist(strsplit(col[i], split = ""))
      if (length(col_split) != 7 | col_split[1] != "#") {
        bad_cols[i] <- TRUE
      }
    }
    if ( any(bad_cols == TRUE) == TRUE ) {
      # print out colors that are 'bad'
      cat( "Some colors are not properly formatted hex codes:",
           paste0("\t",col[bad_cols == TRUE]), sep="\n")
      stop()
    }
  }

  if (is.character(col_alpha) == FALSE) stop("col_alpha must be a character string")
  if (is.numeric(offset) == FALSE) stop("offset must be a numeric value")
  xaxt <- match.arg(xaxt, choices = c("n", "s"))
  yaxt <- match.arg(yaxt, choices = c("n", "s"))


  # Check that fig type is valid
  valid_fig_types <- c("speciation rate",
                       "extinction rate",
                       "net-diversification rate",
                       "relative-extinction rate")

  invalid_fig_types <- fig_types[!fig_types %in% valid_fig_types]

  if ( length(invalid_fig_types) > 0 ) {
    stop("\nThe following figure types are invalid: ", paste(invalid_fig_types, collapse = ", "), ".",
         "\nValid options are: ", paste(valid_fig_types, collapse = ", ") ,".")
  }


  # Make color vector
  if ( is.null(col) ) {
    col <- c("speciation rate"="#984EA3",
             "speciation shift times"="#984EA3",
             "speciation Bayes factors"="#984EA3",
             "extinction rate"="#E41A1C",
             "extinction shift times"="#E41A1C",
             "extinction Bayes factors"="#E41A1C",
             "fossilization rate"="#ffa31a",
             "fossilization shift times"="#ffa31a",
             "fossilization Bayes factors"="#ffa31a",
             "net-diversification rate"="#377EB8",
             "relative-extinction rate"="#FF7F00",
             "mass extinction times"="#4DAF4A",
             "mass extinction Bayes factors"="#4DAF4A")
  } else {
    names(col) <- fig_types
  }

  # Compute the axes
  intervals <- output$`speciation time`[1,grep("interval", colnames(output$`speciation time`))]
  tree_age <- max(intervals)
  num_intervals <- length(intervals) - 1
  plot_at <- 0:num_intervals
  interval_size <- tree_age/num_intervals
  labels <- pretty( c(0,tree_age) ) + offset
  labels_at <- num_intervals - ( pretty(c(0,tree_age)) / interval_size )
  ages <- seq( 0,tree_age, length.out = num_intervals + 1 )

  for( type in fig_types ) {

      this_output <- output[[type]][ ,grep("[0-9]", colnames(output[[type]])) ]
      mean_this_output <- colMeans(this_output)
      quantiles_this_output <- apply(this_output, 2, quantile, prob = c(0.025,0.975))

      # find the limits of the y-axis
      # we always want the speciation and extinction rates to be on the same scale
      if ( type %in% c("speciation rate","extinction rate")) {

        quantiles_speciation <-
          apply(output[["speciation rate"]][ ,grep("[0-9]", colnames(output[["speciation rate"]])) ],
                2, quantile, prob=c(0.025,0.975))
        quantiles_extinction <-
          apply(output[["extinction rate"]][ ,grep("[0-9]", colnames(output[["extinction rate"]])) ],
                2, quantile, prob=c(0.025,0.975))
        ylim <- c( min(0, quantiles_speciation, quantiles_extinction),
                   max(quantiles_speciation, quantiles_extinction) )

      } else {
        ylim <- c( min(0, quantiles_this_output),
                   max(quantiles_this_output) )
      }

      #### plot
        plot( x = plot_at, y = c( rev(mean_this_output)),
              type = "l", ylim = ylim, xaxt = xaxt, col = col[type],
              ylab = "rate", main = type, xlab = xlab,...)
        polygon( x = c( 0:ncol(quantiles_this_output),
                        ncol(quantiles_this_output):0 ),
                 y = c( rev(c(quantiles_this_output[1,1],
                          quantiles_this_output[1,])),
                        c(quantiles_this_output[2,1],
                              quantiles_this_output[2,])),
                 border = NA, col = paste(col[type], col_alpha, sep="") )
        axis(1, at = labels_at, labels = labels)
      }

    }
```

The first step in writing a function is naming the function and establishing the parameters of the function. This is done on the first few lines; in our example, the function is named plotDivRates and contains parameters output, fig_types, etc.. We include ... in the list of parameters to indicate that any additional parameters the user supplies will  be passed as arguments to an internal function called within our function (see below for how this is used). The use of function() tells R that we are defining a function with a name indicated by the assignment arrow, and to expect all parameter names within the parentheses. The opening curly bracket indicates that the function will be defined in the rows below.

```R
plotDivRates <- function(output,
                         fig_types = c("speciation rate",
                                       "extinction rate",
                                       "net-diversification rate",
                                       "relative-extinction rate"),
                         xlab = "million years ago",
                         col = NULL,
                         col_alpha = "50",
                         offset = 0,
                         xaxt = "n",
                         yaxt = "s",
                         ...)
```

We ask that you follow the style guides of RevBayes: functions should be in camel case (i.e. plotDivRates(), processDivRates()) and parameter names should be in snake case (i.e. fig_types, col_alpha). An exception may be made if the parameter will simply be passed to another function (i.e. check.names in the readTrace() function is passed to the read.table() function, and the original read.table() parameter name is preserved).

The tasks that the function will perform are defined within curly brackets following the function() element. Functions should include informative error messages that will catch common user mistakes, especially for correct formating of parameter values. In our example, the first task of the function is to check that the user has indicated logical parameter values. For example, we check that output is a list, fig_types is a string or vector of strings, xlab is a single character string, etc..  We also check that vector lengths match when necessary.

```R
  # Enforce argument matching
  if (is.list(output) == FALSE) stop("output must be list of processed rates")
  if (is.character(fig_types) == FALSE) stop("fig_types must be a character string or vector of strings")
  if (is.character(xlab) == FALSE) stop("xlab must be a single character string")

  # check length of color vector and ensure colors are formatted as hex codes
  if (is.null(col) == FALSE) {
    if (length(col) != length(fig_types))
      stop("col and fig_types must be of the same length")
    bad_cols <- rep("FALSE", times = length(col))
    for (i in 1:length(col)) {
      col_split <- unlist(strsplit(col[i], split = ""))
      if (length(col_split) != 7 | col_split[1] != "#") {
        bad_cols[i] <- TRUE
      }
    }
    if ( any(bad_cols == TRUE) == TRUE ) {
      # print out colors that are 'bad'
      cat( "Some colors are not properly formatted hex codes:",
           paste0("\t",col[bad_cols == TRUE]), sep="\n")
      stop()
    }
  }

  if (is.character(col_alpha) == FALSE) stop("col_alpha must be a character string")
  if (is.numeric(offset) == FALSE) stop("offset must be a numeric value")
  xaxt <- match.arg(xaxt, choices = c("n", "s"))
  yaxt <- match.arg(yaxt, choices = c("n", "s"))


  # Check that fig type is valid
  valid_fig_types <- c("speciation rate",
                       "extinction rate",
                       "net-diversification rate",
                       "relative-extinction rate")

  invalid_fig_types <- fig_types[!fig_types %in% valid_fig_types]

  if ( length(invalid_fig_types) > 0 ) {
    stop("\nThe following figure types are invalid: ", paste(invalid_fig_types, collapse = ", "), ".",
         "\nValid options are: ", paste(valid_fig_types, collapse = ", ") ,".")
  }

```

The stop command is used to provide informative error messages if these checks are false. `match.arg()` ensures that the parameter value is one of only a few designed options. If the user inputs a value that is not included in the choices for `match.arg()`, the function will stop and inform the user that values must be either of the options (`"n"` or `"s"` for `xaxt` and `yaxt` in this example). This works well  for single values, but if the user may supply any number of potential options in a vector, we check that the supplied values fall within our predetermined possible values (in `fig_type`, for example, only four options are currently permitted).

Next, the function performs its task, in our case producing plots from processed log files. We have designed the function to accept user provided colors as hex codes, but if the user does  not specify colors for the plots, we assign them at the beginning. Next, we compute our axes for the plots based on the processed rates and assign labels to the x axes. Finally, we loop through each desired plot type, calculating means and quantiles from the posteriors and plotting the results.

```R
# read in tree(s) of type nexus or newick

# Make color vector
  if ( is.null(col) ) {
    col <- c("speciation rate"="#984EA3",
             "speciation shift times"="#984EA3",
             "speciation Bayes factors"="#984EA3",
             "extinction rate"="#E41A1C",
             "extinction shift times"="#E41A1C",
             "extinction Bayes factors"="#E41A1C",
             "fossilization rate"="#ffa31a",
             "fossilization shift times"="#ffa31a",
             "fossilization Bayes factors"="#ffa31a",
             "net-diversification rate"="#377EB8",
             "relative-extinction rate"="#FF7F00",
             "mass extinction times"="#4DAF4A",
             "mass extinction Bayes factors"="#4DAF4A")
  } else {
    names(col) <- fig_types
  }

  # Compute the axes
  intervals <- output$`speciation time`[1,grep("interval", colnames(output$`speciation time`))]
  tree_age <- max(intervals)
  num_intervals <- length(intervals) - 1
  plot_at <- 0:num_intervals
  interval_size <- tree_age/num_intervals
  labels <- pretty( c(0,tree_age) ) + offset
  labels_at <- num_intervals - ( pretty(c(0,tree_age)) / interval_size )
  ages <- seq( 0,tree_age, length.out = num_intervals + 1 )

  for( type in fig_types ) {

      this_output <- output[[type]][ ,grep("[0-9]", colnames(output[[type]])) ]
      mean_this_output <- colMeans(this_output)
      quantiles_this_output <- apply(this_output, 2, quantile, prob = c(0.025,0.975))

      # find the limits of the y-axis
      # we always want the speciation and extinction rates to be on the same scale
      if ( type %in% c("speciation rate","extinction rate")) {

        quantiles_speciation <-
          apply(output[["speciation rate"]][ ,grep("[0-9]", colnames(output[["speciation rate"]])) ],
                2, quantile, prob=c(0.025,0.975))
        quantiles_extinction <-
          apply(output[["extinction rate"]][ ,grep("[0-9]", colnames(output[["extinction rate"]])) ],
                2, quantile, prob=c(0.025,0.975))
        ylim <- c( min(0, quantiles_speciation, quantiles_extinction),
                   max(quantiles_speciation, quantiles_extinction) )

      } else {
        ylim <- c( min(0, quantiles_this_output),
                   max(quantiles_this_output) )
      }

      #### plot
        plot( x = plot_at, y = c( rev(mean_this_output)),
              type = "l", ylim = ylim, xaxt = xaxt, col = col[type],
              ylab = "rate", main = type, xlab = xlab,...)
        polygon( x = c( 0:ncol(quantiles_this_output),
                        ncol(quantiles_this_output):0 ),
                 y = c( rev(c(quantiles_this_output[1,1],
                          quantiles_this_output[1,])),
                        c(quantiles_this_output[2,1],
                              quantiles_this_output[2,])),
                 border = NA, col = paste(col[type], col_alpha, sep="") )
        axis(1, at = labels_at, labels = labels)
      }
}

```

In many cases, you may want the function to produce  an object (data frame, value, etc.) that will be outputted to the user. In `plotDivRates()`, we do not do this; the function simply plots the provided data, and the final curly bracket ends the function. However, in `processDivRates()`, we wish to return a list of the process rates. We create a list of the processed rates, and then use `return()` to indicate that the function should produce this for the user.

```R

 # return a list of processed data frames
 res <- list("speciation rate" = speciation_rate,
             "extinction rate" = extinction_rate,
             "net-diversification rate" = net_diversification_rate,
             "relative-extinction rate" = relative_extinction_rate,
             "speciation time" = speciation_time,
             "extinction time" = extinction_time)
 return(res)

```


## Test Function

We ask that you additionally include a separate file that can be used to test the function using `testthat`. These files should be stored in the RevGadgets > tests > testthat subdirectory. For the `processDivRates()` function, we have created a testing file called `test_processDivRates.R`, the contents of which are shown below:

```R
context("tests the processDivRates function")

# relies heavily on readTrace(), so we only test for elements not
# included in the readTrace() testing.

test_that("processes birth-death scripts", {
  file_spectimes <- system.file("extdata", "epi_bd/primates_EBD_speciation_times.p", package="RevGadgets")
  file_specrates <- system.file("extdata", "epi_bd/primates_EBD_speciation_rates.p", package="RevGadgets")
  file_exttimes <- system.file("extdata", "epi_bd/primates_EBD_extinction_times.p", package="RevGadgets")
  file_extrates <- system.file("extdata", "epi_bd/primates_EBD_extinction_rates.p", package="RevGadgets")

  primates <- processDivRates(speciation_time_log = file_spectimes,
                              speciation_rate_log = file_specrates,
                              extinction_time_log = file_exttimes,
                              extinction_rate_log = file_extrates,
                              burnin = 0.25)
  expect_equal(length(primates), 6)
  expect_equal(class(primates), "list")
  expect_equal(nrow(primates[[1]]), 3750)
})

```

Each function should have an associated testing script that tests various functionalities. As the creator of the function, it is up to you to choose which types of tests will be most informative in catching potential errors. We recommend erring on the side of including tests that seem redundant or unnecessary, because changes in dependency packages, or other unexpected events, can cause errors that you may not expect. We also recommend that if you ever find a bug in your code, you design a test to catch the bug if it were to ever happen again.

The `test_that()` function defines a test that checks that the outcome of your function matches what you expect. In this example, we define one test for reading in a set of four log files.

This test reads in the log files and then checks three aspects of the function:

1) The result contains 6 elements
2) The class of the resulting object is of type list
3) There are the expected number of rows (generations) in the processed data frames

In many cases, our tests will rely on reading in sample datasets. The outputs of many RevBayes tutorials are saved in the `inst/extdata` folder of the RevGadgets directory. Please use these standard files for testing and examples. Note that log files in these example datasets are referred to with `.p` to faciliate unit testing with Travis. Similarly, we refer to files using the `system.file()` function to ensure that paths are consistent regardless of user installation.

### Testing plotting functions

We recommend that plotting functions are tested by comparing a saved version of the gg object to the newly produced gg object. Once the function is producing a plot that you are content with, save the plot object as a `.Rdata` file in `inst/extdata/graphs`. This file can then be loaded in during testing and compared with a newly-produced plot object. For example, the follow test compares a saved version and new version of the `plotTrace()` plot.

```R

  # load in the trace file
  file_1 <- system.file("extdata",
                      "sub_models/primates_cytb_GTR.p",
                      package="RevGadgets")
  one_trace <- readTrace(path = file_1)
  # produce the plot pi parameters object
  plots_new <- plotTrace(trace = one_trace,
                     vars = c("pi[1]","pi[2]","pi[3]","pi[4]"))
  # load the saved plot for comparison
  file_2 <- system.file("extdata",
                        "graphs/plotTrace_pi.Rdata",
                        package="RevGadgets")
  load(file_2) # loads an object called 'plots'
  expect_equal(plots[[1]], plots_new[[1]])
  })

```
## Unit Testing and GitHub

When you push to GitHub, GitHub will automatically run a series of tests on the updated package. This include running our designed tests using `testthat`, but also includes checks of package  compatibility, appropriate documention, etc. This will ensure that any changes to the development branch do not break basic package functionality.

If have any questions about these recommendations or are hesitant about pushing to the development branch, feel free to contact the developers for additional guidance.

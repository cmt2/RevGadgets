#' Plot Diversification Rates
#'
#' Plots the output of an episodic diversification rate analysis
#'
#' Plots the output of episodic diversification rate analyses. Takes as
#' input the output of processDivRates() and plotting parameters.
#' For now, only variable names (under "item") that contain the word "rate" are
#' included in the plot.
#'
#' The return object can be manipulated. For example, you can change the
#' axis labels, the color palette, whether the axes are to be linked, or the
#' overall plotting style/theme, just as with any ggplot object.
#'
#'
#' @param rates (list of dataframes; no default) A list of dataframes,
#' such as produced by processDivRates(), containing the data on rates
#' and interval times for each type of rate to be plotted (e.g.
#' speciation rate, etc.).
#'
#' @param facet (logical; TRUE) plot rates in separate facets.
#'
#'
#' @return A ggplot object
#'
#' @examples
#'
#' \donttest{
#' # download the example datasets to working directory
#'
#' url_ex_times <-
#'    "https://revbayes.github.io/tutorials/intro/data/primates_EBD_extinction_times.log"
#' dest_path_ex_times <- "primates_EBD_extinction_times.log"
#' download.file(url_ex_times, dest_path_ex_times)
#'
#' url_ex_rates <-
#'    "https://revbayes.github.io/tutorials/intro/data/primates_EBD_extinction_rates.log"
#' dest_path_ex_rates <- "primates_EBD_extinction_rates.log"
#' download.file(url_ex_rates, dest_path_ex_rates)
#'
#' url_sp_times <-
#'    "https://revbayes.github.io/tutorials/intro/data/primates_EBD_speciation_times.log"
#' dest_path_sp_times <- "primates_EBD_speciation_times.log"
#' download.file(url_sp_times, dest_path_sp_times)
#'
#' url_sp_rates <-
#'    "https://revbayes.github.io/tutorials/intro/data/primates_EBD_speciation_rates.log"
#' dest_path_sp_rates <- "primates_EBD_speciation_rates.log"
#' download.file(url_sp_rates, dest_path_sp_rates)
#'
#' # to run on your own data, change this to the path to your data file
#' speciation_time_file <- dest_path_sp_times
#' speciation_rate_file <- dest_path_sp_rates
#' extinction_time_file <- dest_path_ex_times
#' extinction_rate_file <- dest_path_ex_rates
#'
#' rates <- processDivRates(speciation_time_log = speciation_time_file,
#'                          speciation_rate_log = speciation_rate_file,
#'                          extinction_time_log = extinction_time_file,
#'                          extinction_rate_log = extinction_rate_file,
#'                          burnin = 0.25)
#'
#' # then plot results:
#' p <- plotDivRates(rates = rates);p
#'
#' # change the x-axis
#' p <- p + ggplot2::xlab("Thousands of years ago");p
#'
#' # change the colors
#' p <- p + ggplot2::scale_fill_manual(values = c("red",
#'                                                "green",
#'                                                "yellow",
#'                                                "purple")) +
#'   ggplot2::scale_color_manual(values = c("red",
#'                                          "green",
#'                                          "yellow",
#'                                          "purple"));p
#'
#' # let's say we don't want to plot relative-extinction rate,
#' # and use the same y-axis for all three rates
#' rates <- rates[!grepl("relative-extinction", rates$item),]
#' p2 <- plotDivRates(rates)
#' p2 <- p2 + ggplot2::facet_wrap(ggplot2::vars(item), scale = "fixed");p2
#'
#' # remove files
#' # WARNING: only run for example dataset!
#' # otherwise you might delete your data!
#' file.remove(dest_path_sp_times, dest_path_ex_times,
#'             dest_path_sp_rates, dest_path_ex_rates)
#'
#' }
#'
#' @export

plotDivRates <- function(rates, facet = TRUE){
  message("Using default time units in x-axis label: Age (Ma)")
  rates_to_plot <- unique(rates$item)[grep("rate", unique(rates$item))]
  
  `%>%` <- dplyr::`%>%`
  
  p <- rates %>%
    subset(grepl("rate", item)) %>%
    ggplot2::ggplot(ggplot2::aes(time, value, color = item))  +
    ggplot2::geom_line() +
    ggplot2::geom_ribbon(aes(ymin = lower, ymax = upper, fill = item),
                         alpha = 0.4,
                         color = NA) +
    ggplot2::scale_x_reverse() +
    ggplot2::xlab("Age (Ma)") +
    ggplot2::ylab("Rate") +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.title = ggplot2::element_blank(),
                   legend.position = "none",
                   panel.grid.major = ggplot2::element_blank(),
                   panel.grid.minor = ggplot2::element_blank(),
                   strip.background = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = colFun(length(rates_to_plot))) +
    ggplot2::scale_fill_manual(values = colFun(length(rates_to_plot)))
  
  if (facet){
    p <- p +
      ggplot2::facet_wrap(dplyr::vars(item),
                          scales = "free_y",
                          labeller =
                            ggplot2::labeller(item = .titleFormatLabeller))
  }
  
  
  return(p)
}
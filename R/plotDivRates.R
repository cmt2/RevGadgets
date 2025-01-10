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
#' @param rates a dataframe, such as produced by processDivRates(),
#' containing the data on rates and interval times for each type of 
#' rate to be plotted (e.g. speciation rate, etc.).
#' @param env_age (optional) a vector of time points for an environmental variable
#' @param env_var (optional) a vector of values for an environmental variable
#' @param env_label (optional) a label for the environmental variable
#' @param env_scaling (optional) the ratio between the tick values for the 
#' primary (left-hand, diversification rates) y-axis and the 
#' secondary (right-hand, environmental variable) y-axis 
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
#' ggplot2::scale_color_manual(values = c("red",
#'                                        "green",
#'                                        "yellow",
#'                                        "purple"));p
#'
#' # let's say we don't want to plot relative-extinction rate,
#' # and use the same y-axis for all three rates
#' rates <- rates[!grepl("relative-extinction", rates$item),]
#' p2 <- plotDivRates(rates)
#' p2 <- p2 + ggplot2::facet_wrap(ggplot2::vars(item), scale = "fixed");p2
#' 
#' ## suppose we want to add a secondary axis showing an environmental variable
#' co2 <- c(297.6, 301.36, 304.84, 307.86, 310.36, 312.53, 314.48, 
#'          316.31, 317.42, 317.63, 317.74, 318.51, 318.29, 316.5, 
#'          315.49, 317.64, 318.61, 316.6, 317.77, 328.27, 351.12, 
#'          381.87, 415.47, 446.86, 478.31, 513.77, 550.74, 586.68, 
#'          631.48, 684.13, 725.83, 757.81, 789.39, 813.79, 824.25, 
#'          812.6, 784.79, 755.25, 738.41, 727.53, 710.48, 693.55, 
#'          683.04, 683.99, 690.93, 694.44, 701.62, 718.05, 731.95, 
#'          731.56, 717.76)
#'          
#' co2_age <- seq(0.0, 50.0, length.out = length(co2))
#' 
#' p3 <- plotDivRates(rates, env_age = co2_age, env_var = co2, 
#'                    env_label = "co2 (ppm)", env_scaling = 1000)
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

plotDivRates <- function(rates, env_age = NULL, env_var = NULL, env_label = "env_label", env_scaling = 1.0, facet = TRUE){
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
  
  if (any(c(!is.null(env_age), !is.null(env_var)))){
    if(is.null(env_age)){
      stop("must set env_age")
    }
    if(is.null(env_var)){
      stop("must set env_var")
    }
    if(length(env_age) != length(env_var)){
      stop("env_var and env_age must be same length")
    }
    
    foo <- function(item) data.frame("age" = env_age, "env" = env_var, "item" = rep(item, length(env_var)))
    
    env_dummy <- lapply(unique(rates$item), foo)
    env_df <- do.call(rbind, env_dummy)
  
  p <- p + ggplot2::geom_line(aes(y = env / env_scaling, x = age), 
                     color = "black", linetype = "dashed", data = env_df) +
    ggplot2::scale_y_continuous(
      "Rate", 
      sec.axis = ggplot2::sec_axis(~ . * env_scaling, name = env_label)
    )
  }

  
  return(p)
}
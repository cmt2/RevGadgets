#' plotStochMaps
#'
#' @param tree (treedata object; none) Output of readTrees() function
#' containing tree.
#' @param maps (dataframe; no default) Dataframe with processed maps,
#' as in the output of processStochMaps()
#' @param colors (named character vector; no default) Named character vector
#' where items are colors and names are the corresponding states.
#' 
#' @param color_by (character string; "prob") How to color the branches. 
#' Options are "MAP" for assigning color by the MAP state, or "prob" for 
#' assigning color as the weighted average of states based on the posterior
#' probabilities.
#'
#' @param timeline (logical; FALSE) Plot time tree with labeled x-axis with
#' timescale in MYA.
#'
#' @param geo (logical; timeline) Add a geological timeline? Defaults to the
#' same as timeline.
#'
#' @param time_bars (logical; timeline) Add vertical gray bars to indicate
#' geological timeline units if geo == TRUE or regular time intervals (in MYA)
#' if geo == FALSE.
#'
#' @param geo_units (list; list("epochs", "periods")) Which geological units to
#' include in the geo timescale. May be "periods", "epochs", "stages", "eons",
#' "eras", or a list of two of those units.
#'
#' @param tip_labels (logical; TRUE) Plot tip labels?
#'
#' @param tip_labels_italics (logical; FALSE) Plot tip labels in italics?
#'
#' @param tip_labels_formatted (logical; FALSE) Do the tip labels contain
#' manually added formatting information? Will set parse = TRUE in geom_text()
#' and associated functions to interpret formatting. See ?plotmath for more.
#' Cannot be TRUE if tip_labels_italics = TRUE.
#'
#' @param tip_labels_remove_underscore (logical; TRUE) Remove underscores in tip
#' labels?
#'
#' @param tip_labels_color (character; "black") Color to plot tip labels, either
#' as a valid R color name or a valid hex code.
#'
#' @param tip_labels_size (numeric; 3) Size of tip labels
#'
#' @param tip_labels_offset (numeric; 1) Horizontal offset of tip labels from
#' tree.
#'
#' @param line_width (numeric; 1) Change line width for branches
#'
#' @param tree_layout (character; "rectangular") Tree shape layout, passed
#' to ggtree(). Options are 'rectangular', 'cladogram', 'slanted', 'ellipse',
#' 'roundrect', 'fan', 'circular', 'inward_circular', 'radial', 'equal_angle',
#' 'daylight', or 'ape'.
#' 
#' @param label_sampled_ancs (logical; FALSE) Label any sampled ancestors?
#' Will inherent tip labels aesthetics for size and color.
#'
#' @param ... (various) Additional arguments passed to ggtree::ggtree().
#'
#' @return returns a single plot object.
#'
#' @examples
#'
#' \donttest{
#'
#' # Standard stochastic mapping example
#' 
#' # read a tree (REPLACE WITH DOWNLOADING EXAMPLE BEFORE PUBLISHING)
#' treefile <- system.file("extdata",
#'                         "stoch_map_test_tmp/tree.nexus",
#'                         package="RevGadgets")
#'                         
#' tree <- readTrees(treefile)[[1]][[1]]
#' 
#' # process samples
#' mapsfile <- system.file("extdata",
#'                         "stoch_map_test_tmp/maps.log",
#'                         package="RevGadgets")
#'                         
#' stoch_map_df <- processStochMaps(tree,
#'                                  mapsfile, 
#'                                  states = as.character(0:4), 
#'                                  burnin = 0.1)
#' 
#' plotStochMaps(tree = tree,
#'               maps = stoch_map_df,
#'               color_by = "MAP",
#'               colors = "default",
#'               tree_layout = "rectangular",
#'               tip_labels = FALSE)
#'
#' }
#'
#' @export

plotStochMaps <- function(tree,
                          maps,
                          colors = "default",
                          color_by = "prob",
                          tree_layout = "rectangular",
                          line_width = 1,
                          tip_labels = TRUE,
                          tip_labels_italics = FALSE,
                          tip_labels_formatted = FALSE,
                          tip_labels_remove_underscore = TRUE,
                          tip_labels_color = "black",
                          tip_labels_size = 3,
                          tip_labels_offset = 0,
                          timeline = FALSE,
                          geo_units = list("epochs", "periods"),
                          geo = timeline,
                          time_bars = timeline,
                          label_sampled_ancs = FALSE,
                          ...) {
  
  # pull tree from list object if necessary
  if (inherits(tree,"list")) {
    if (length(tree) == 1){
      tree <- tree[[1]]
    } else {stop("tree should contain only one tree object")}
  }
  
  if (inherits(tree,"list")) {
    if (length(tree) == 1){
      tree <- tree[[1]]
    } else {stop("tree should contain only one tree object")}
  } 
  
  p <-  plotTreeFull(
    tree = list(list(tree)),
    tree_layout = tree_layout,
    line_width = line_width,
    
    tip_labels = tip_labels,
    tip_labels_italics = tip_labels_italics,
    tip_labels_formatted = tip_labels_formatted,
    tip_labels_remove_underscore = tip_labels_remove_underscore,
    tip_labels_color = tip_labels_color,
    tip_labels_size = tip_labels_size,
    tip_labels_offset = tip_labels_offset,
    
    timeline = timeline,
    geo_units = geo_units,
    geo = timeline,
    time_bars = timeline,
    
    label_sampled_ancs = label_sampled_ancs,
    
    node_age_bars = FALSE,
    age_bars_color = "blue",
    age_bars_colored_by = NULL,
    age_bars_width = 1,
    
    node_labels = NULL,
    node_labels_color = "black",
    node_labels_size = 3,
    node_labels_offset = 0,
    
    node_pp = FALSE,
    node_pp_shape = 16,
    node_pp_color = "black",
    node_pp_size = "variable",
    
    branch_color = "black",
    color_branch_by = NULL,
    
    tip_age_bars = FALSE,
    lineend = "square",
    ...
  )

  if (colors[1] != "default") {
    states <- names(colors)
  } else {
    states <- colnames(maps)[-c(1:5)]
    colors <- colFun(length(states))
    names(colors) <- states
  }
  
  dat <- dplyr::left_join(maps, p$data, by = "node")
  
  #set up colors 
  if (color_by == "MAP") {
    max <- apply(dat[, states], MARGIN = 1, which.max)
    seg_col <- colors[unlist(max)]
    dat$seg_col <- seg_col
    names(seg_col) <- seg_col
  } else if (color_by == "prob") {
    rgbcols <- col2rgb(colors)
    rgb_values_per_seg <- t(rgbcols %*% t(dat[,states]))
    seg_col <- tolower(grDevices::rgb(red   = rgb_values_per_seg[ ,1],
                                      green = rgb_values_per_seg[ ,2],
                                      blue  = rgb_values_per_seg[ ,3],
                                      maxColorValue = 255))
    dat$seg_col <- seg_col
    names(seg_col) <- seg_col
  }

  # horizontal segments
  dat_horiz <- dat[dat$vert == FALSE,]
  
  seg_horiz <- data.frame(
    x    = dat_horiz$x - dat_horiz$x0,
    xend = dat_horiz$x - dat_horiz$x1,
    y    = dat_horiz$y,
    yend = dat_horiz$y,
    col  = dat_horiz$seg_col
  )
  
  #vertical segments
  dat_vert <- dat[dat$vert == TRUE,]
  
  m <- match(x = dat_vert$parent, dat_vert$node)
  dat_vert$y_parent <- dat_vert[m, "y"]
  dat_vert$x_parent <- dat_vert[m, "x"]
  
  seg_vert <- data.frame(
    x = dat_vert$x_parent,
    xend = dat_vert$x_parent,
    y = dat_vert$y,
    yend = dat_vert$y_parent,
    col = dat_vert$seg_col
  )
  
  # plot! 
  
  p + ggplot2::geom_segment(
    data = seg_horiz,
    ggplot2::aes(
      x = x,
      y = y,
      xend = xend,
      yend = yend,
      color = col
    ),
    lineend = "square",
    size = line_width,
  ) +
    ggplot2::geom_segment(
      data = seg_vert,
      ggplot2::aes(
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        color = col
      ),
      lineend = "square",
      size = line_width, 

    ) +
    ggplot2::scale_color_manual(values = seg_col, 
                                breaks = colors,
                                name = "State",
                                labels = names(colors))
    
}

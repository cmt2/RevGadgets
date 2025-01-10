#' plot Ancestral States Pie
#'
#' Plot character states and posterior probabilities as pies on nodes.
#'
#' @param t (treedata object; none) Output of processAncStates() function
#' containing tree and ancestral states.
#' @param cladogenetic (logical; FALSE) Plot shoulder pies of cladogenetic
#' analyses?
#' @param tip_labels (logical; TRUE) Label taxa labels at tips?
#' @param tip_labels_size (numeric; 2) Size of tip labels.
#' @param tip_labels_offset (numeric; 1) Horizontal offset of tip labels from
#' tree.
#' @param tip_labels_italics (logical; FALSE) Italicize tip labels?
#' @param tip_labels_remove_underscore (logical; TRUE) Remove underscores from
#' tip labels?
#' @param tip_labels_formatted (logical; FALSE) Do the tip labels contain 
#' manually added formatting information? Will set parse = TRUE in geom_text()
#' and associated functions to interpret formatting. See ?plotmath for more.
#' Cannot be TRUE if tip_labels_italics = TRUE.  
#' @param tip_labels_states (logical; FALSE) Optional plotting of text at tips
#' in addition to taxa labels. Will plot the MAP state label.
#' @param tip_labels_states_size (numeric; 2) Size of state labels at tips.
#' Ignored if tip_labels_states is FALSE.
#' @param tip_labels_states_offset (numeric; 0.1) Horizontal offset of tip
#' state labels. Ignored if tip_labels_states = NULL.
#' @param node_labels_as (character; NULL) Optional plotting of text at nodes.
#' Possible values are "state" for the MAP ancestral states, "node_posterior"
#' for the posterior probability of the node on the tree, “state_posterior” 
#' for the posterior probability of the MAP, or NULL for not plotting any
#' text at the nodes (default).
#' @param node_labels_size (numeric; 2) Size of node labels text. Ignored if
#' node_labels_as = NULL.
#' @param node_labels_offset (numeric; 0.1) Horizontal offset of node labels
#' from nodes. Ignored if node_labels_as = NULL.
#' @param pie_colors ("character"; "default") Colors for states in pies.
#' If "default", plots the default RevGadgets colors. Provide a character
#' vector of hex codes or other R-readable colors the same length of the number
#' of character states. Names of the vector should correspond to state labels.
#' @param node_pie_size (numeric; 1) Size (diameter) of the pies at nodes.
#' @param tip_pie_size (numeric; 0.5) Size (diameter) of the pies at tips.
#' @param shoulder_pie_size (numeric; node_pie_size) Size (diameter) of the
#' pies at shoulders for cladogenetic plots.
#' @param tip_pies (logical; TRUE) Plot pies tips?
#' @param node_pie_nudge_x (numeric; 0) If pies aren't centered, adjust by
#' nudging
#' @param node_pie_nudge_y (numeric; 0) If pies aren't centered, adjust by
#' nudging
#' @param tip_pie_nudge_x (numeric; node_pie_nudge_x) If pies aren't centered,
#' adjust by nudging
#' @param tip_pie_nudge_y (numeric; node_pie_nudge_y) If pies aren't centered,
#' adjust by nudging
#' @param shoulder_pie_nudge_x (numeric; node_pie_nudge_x) If pies aren't
#' centered, adjust by nudging
#' @param shoulder_pie_nudge_y (numeric; node_pie_nudge_y) If pies aren't
#' centered, adjust by nudging
#' @param state_transparency (integer; 0.75) Alpha (transparency) of state
#' symbols- varies from 0 to 1.
#' @param timeline (logical; FALSE) Plot tree with labeled x-axis with
#' timescale in MYA.
#' @param geo (logical; timeline) Add a geological timeline? Defaults to the
#' same as timeline.
#' @param time_bars (logical; timeline) Add vertical gray bars to indicate
#' geological timeline units if geo == TRUE or regular time intervals (in MYA)
#' if geo == FALSE.
#' @param geo_units (list; list("epochs", "periods")) Which geological units to
#' include in the geo timescale. May be "periods", "epochs", "stages", "eons", 
#' "eras", or a list of two of those units.
#' @param ... (various) Additional arguments passed to ggtree::ggtree().
#'
#' @return A ggplot object
#'
#' @examples
#'
#' \donttest{
#'
#' # Standard ancestral state reconstruction example
#'
#' # process file and assign state labels
#' file <- system.file("extdata",
#'                     "comp_method_disc/ase_freeK.tree",
#'                     package="RevGadgets")
#' example <- processAncStates(file,
#'                             state_labels = c("1" = "Awesome",
#'                                              "2" = "Beautiful",
#'                                              "3" = "Cool!"))
#' # plot (this may take a while)
#' plotAncStatesPie(t = example)
#'
#' # DEC Biogeographic range evolution example (with timeline)
#'
#' # process file
#' file <- system.file("extdata", "dec/simple.ase.tre", package="RevGadgets")
#'
#' # labels that correspond to each region/ possible combination of regions
#' labs <- c("1" = "K", "2" = "O", "3" = "M", "4" = "H", "5" = "KO",
#'           "6" = "KM", "7" = "OM", "8" = "KH", "9" = "OH", "10" = "MH",
#'           "11" = "KOM", "12" = "KOH", "13" = "KMH", "14" = "OMH",
#'           "15" = "KOMH")
#' dec_example  <- processAncStates(file , state_labels = labs)
#' # Use the state_labels in returned tidytree object to define color palette
#' # These state_labels may be a subset of the labels you provided
#' # (not all possible regions may be sampled in the dataset)
#' colors <- colorRampPalette(colFun(12))(length(dec_example@state_labels))
#' names(colors) <- dec_example@state_labels
#'
#' # plot
#' plotAncStatesPie(t = dec_example, pie_colors = colors, tip_labels_size = 3,
#'         cladogenetic = TRUE, tip_labels_offset = 0.25, timeline = TRUE,
#'         geo = FALSE) +
#'         ggplot2::theme(legend.position = c(0.1, 0.75))
#' }
#'
#' @export

plotAncStatesPie <- function(t,
                             # option for plotting shoulder states
                             cladogenetic = FALSE,

                             # label taxa at tips
                             tip_labels = TRUE,
                             tip_labels_size = 2,
                             tip_labels_offset = 1,
                             tip_labels_italics = FALSE,
                             tip_labels_formatted = FALSE,
                             tip_labels_remove_underscore = TRUE,

                             # label states at tips
                             tip_labels_states = FALSE,
                             tip_labels_states_size = 2,
                             tip_labels_states_offset = 0.1,

                             # text labels at nodes
                             node_labels_as = NULL,
                             node_labels_size = 2,
                             node_labels_offset = 0.1,

                             # pies aesthetics
                             pie_colors = "default",
                             node_pie_size = 1,
                             shoulder_pie_size = node_pie_size,
                             tip_pies = TRUE,
                             tip_pie_size = 0.5,

                             # nudges to center pies
                             node_pie_nudge_x = 0,
                             node_pie_nudge_y = 0,
                             tip_pie_nudge_x = node_pie_nudge_x,
                             tip_pie_nudge_y = node_pie_nudge_y,
                             shoulder_pie_nudge_x = node_pie_nudge_x,
                             shoulder_pie_nudge_y = node_pie_nudge_y,

                             state_transparency = 0.75,

                             timeline = FALSE,
                             geo = timeline,
                             geo_units = list("epochs", "periods"),
                             time_bars = timeline,
                             ...) {
  ##### parameter compatibility checks #####
  if (!methods::is(t, "treedata"))
    stop("t should be a treedata object")
  if (is.logical(cladogenetic) == FALSE)
    stop("cladogenetic should be TRUE or FALSE")
  if (is.logical(tip_labels) == FALSE)
    stop("tip_labels should be TRUE or FALSE")
  if (is.numeric(tip_labels_size) == FALSE)
    stop("tip_labels_size should be a number")
  if (is.numeric(tip_labels_offset) == FALSE)
    stop("tip_labels_offset should be a number")
  if (is.logical(tip_labels_italics) == FALSE)
    stop("tip_labels_italics should be TRUE or FALSE")
  if (is.logical(tip_labels_formatted) == FALSE)
    stop("tip_labels_formatted should be TRUE or FALSE")
  if (tip_labels_italics == TRUE & tip_labels_formatted == TRUE) 
    stop("tip_labels_italics and tip_labels_formatted may not both be TRUE")
  if (is.logical(tip_labels_remove_underscore) == FALSE)
    stop("tip_labels_remove_underscore should be TRUE or FALSE")
  if (is.logical(tip_labels_states) == FALSE)
    stop("tip_labels_states should be TRUE or FALSE")
  if (is.numeric(tip_labels_states_size) == FALSE)
    stop("tip_labels_states_size should be a number")
  if (is.numeric(tip_labels_states_offset) == FALSE)
    stop("tip_labels_states_offsetshould be a number")
  if (is.null(node_labels_as) == FALSE) {
    node_labels_as <-
      match.arg(node_labels_as,
                choices = c("state", "state_posterior", "node_posterior"))
  }
  if (is.numeric(node_labels_size) == FALSE)
    stop("node_labels_size should be a number")
  if (is.numeric(node_labels_offset) == FALSE)
    stop("node_labels_offset should be a number")
  if (is.character(pie_colors) == FALSE)
    stop ("pie_colors should be 'default' or valid color(s)")
  if (pie_colors[1] != "default" &
      any(.isColor(pie_colors) == FALSE))
    stop("pie_colors should be valid color(s)")
  if (any(is.numeric(node_pie_size) == FALSE))
    stop("node_pie_size should be a single number")
  if (length(node_pie_size) > 1)
    stop("node_pie_size should be a single number")
  if (any(is.numeric(shoulder_pie_size) == FALSE))
    stop("shoulder_pie_size should be a single number")
  if (length(shoulder_pie_size) > 1)
    stop("shoulder_pie_size should be a single number")
  if (any(is.numeric(tip_pie_size) == FALSE))
    stop("tip_pie_size should be a single number")
  if (length(tip_pie_size) > 1)
    stop("tip_pie_size should be a single number")
  if (is.numeric(node_pie_nudge_x) == FALSE)
    stop("node_pie_nudge_x should be a single number")
  if (is.numeric(node_pie_nudge_y) == FALSE)
    stop("node_pie_nudge_y should be a single number")
  if (is.numeric(tip_pie_nudge_x) == FALSE)
    stop("tip_pie_nudge_x should be a single number")
  if (is.numeric(tip_pie_nudge_y) == FALSE)
    stop("tip_pie_nudge_y should be a single number")
  if (is.numeric(shoulder_pie_nudge_x) == FALSE)
    stop("shoulder_pie_nudge_x should be a single number")
  if (is.numeric(shoulder_pie_nudge_y) == FALSE)
    stop("shoulder_pie_nudge_y should be a single number")
  if (length(node_pie_nudge_x) != 1)
    stop("node_pie_nudge_x should be a single number")
  if (length(node_pie_nudge_y) != 1)
    stop("node_pie_nudge_y should be a single number")
  if (length(tip_pie_nudge_x) != 1)
    stop("tip_pie_nudge_x should be a single number")
  if (length(tip_pie_nudge_y) != 1)
    stop("tip_pie_nudge_y should be a single number")
  if (length(shoulder_pie_nudge_x) != 1)
    stop("shoulder_pie_nudge_x should be a single number")
  if (length(shoulder_pie_nudge_y) != 1)
    stop("shoulder_pie_nudge_y should be a single number")
  if (is.numeric(state_transparency) == FALSE)
    stop("state_transparency should be a number between 0 - 1")
  if (state_transparency < 0 ||
      state_transparency > 1)
    stop("state_transparency should be a number between 0 - 1")
  if (is.logical(timeline) == FALSE)
    stop("timeline should be TRUE or FALSE")
  if (is.list(geo_units)) {
    if (length(geo_units) != 2)
      stop(
        "geo_units should be 'periods', 'epochs', 'stages', 'eons', 
         'eras', or a list of two of those units, such as:
        list('epochs','periods')"
      )
    if (geo_units[[1]] %in% 
        c('periods', 'epochs', 'stages', 'eons', 'eras')  == FALSE)
      stop(
        "geo_units should be 'periods', 'epochs', 'stages', 'eons', 
         'eras', or a list of two of those units, such as:
        list('epochs','periods')"
      )
    if (geo_units[[2]] %in% 
        c('periods', 'epochs', 'stages', 'eons', 'eras')  == FALSE)
      stop(
        "geo_units should be 'periods', 'epochs', 'stages', 'eons', 
         'eras', or a list of two of those units, such as:
        list('epochs','periods')"
      )
  } else {
    if (geo_units %in% 
        c('periods', 'epochs', 'stages', 'eons', 'eras') == FALSE)
      stop(
        "geo_units should be 'periods', 'epochs', 'stages', 'eons', 
         'eras', or a list of two of those units, such as:
        list('epochs','periods')"
      )
  }

  ##### create basic tree plot #####
  p <- ggtree::ggtree(t, ...)

  ##### specify temp directory for intermediary files #####
  tmp <- tempdir()

  ##### calculate helper variables #####
  tree <- attributes(t)$phylo
  tree_height <- max(phytools::nodeHeights(t@phylo))
  n_node <- ape::Nnode(tree, internal.only = FALSE)
  ntips <- length(tree$tip.label)
  node_idx <- (ntips + 1):n_node
  tip_idx <- 1:ntips
  all_idx <- 1:n_node

  ##### transform nudge parameter #####
  tip_pie_nudge_x <- -tip_pie_nudge_x
  node_pie_nudge_x <- -node_pie_nudge_x
  shoulder_pie_nudge_x <- -shoulder_pie_nudge_x

  ##### reorder labels #####
  state_labels <- as.factor(attributes(t)$state_labels)

  ##### calculate pie sizes #####
  node_pie_size <-  node_pie_size / 30
  shoulder_pie_size <- shoulder_pie_size / 30
  tip_pie_size <- tip_pie_size / 30

  if (cladogenetic == TRUE) {
    state_pos_str_base <- c("end_state_", "start_state_")
  } else if (cladogenetic == FALSE &
             "start_state_1" %in% colnames(p$data)) {
    state_pos_str_base <- "end_state_"
  } else if (cladogenetic == FALSE &
             "anc_state_1" %in% colnames(p$data)) {
    state_pos_str_base <- "anc_state_"
  }

  ##### color and label processing #####

  # check if number of states exceeds default color palette options
  if (pie_colors[1] == "default") {
    nstates <- length(state_labels)
    if (nstates <= 12) {
      pie_colors <- colFun(nstates)
    } else {
      pie_colors <- grDevices::colorRampPalette(colFun(12))(nstates)
    }
  }

  # check if number of states not equal to provided colors
  if (pie_colors[1] != "default" &
      length(pie_colors) < length(state_labels)) {
    stop(
      paste0(
        "You provided fewer colors in node_color
                than states in your dataset. There are ",
        length(state_labels),
        " states and you provide ",
        length(pie_colors),
        " colors."
      )
    )
  }

  # add names to colors if none present
  if (is.null(names(pie_colors))) {
    names(pie_colors) <- state_labels
  }

  # set colors, add "other" if necessary
  otherpp <- as.numeric(dplyr::pull(p$data,
                                    var = paste0(state_pos_str_base[1],
                                                 "other_pp")))
  if (sum(otherpp, na.rm = TRUE) == 0) {
    # set default colors
    if (any(pie_colors == "default")) {
      nstates <- length(state_labels)
      colors <- c(colFun(nstates))
      names(colors) <- state_labels
    } else {
      colors <- pie_colors
    }

  } else if (sum(otherpp, na.rm = TRUE) != 0) {
    
    state_labels <- as.factor(c(as.character(t@state_labels), "other"))

    if ("anc_state_" %in% state_pos_str_base) {
      p$data$anc_state_other <- "other"
    }
    if ("end_state_" %in% state_pos_str_base) {
      p$data$end_state_other <- "other"
    }

    # add other to user-set colors
    if (pie_colors[1] != "default") {
      pc_names <- names(pie_colors)
      pie_colors <- c(pie_colors, "grey50")
      names(pie_colors) <- c(pc_names, "other")
    }

    # set default colors
    if (any(pie_colors == "default")) {
      nstates <- length(state_labels) - 1
      colors <- c(colFun(nstates), "grey50")
      names(colors) <- state_labels
    } else {
      colors <- pie_colors
    }

  }

  ##### reformat labels if necessary #####
  if (tip_labels_remove_underscore) {
    p$data$label <- gsub("_", " ", p$data$label)
  }

  ##### start plotting #####

  # add timeline
  if (timeline == TRUE) {

    max_age <- tree_height

    if (max_age > 100) {
      interval <- 50
    } else {
      interval <- 10
    }
    dx <- max_age %% interval
    # set coordinates
    ### Fix the xlim and ylims - if no error bars, should be a function of
    ### max age and n nodes, respectively.
    ### If error bars, -x lim should be as old as the max of the error bar.
    tick_height <- ntips / 100
    if (geo == TRUE) {
      #determine whether to include quaternary
      if (tree_height > 50) {
        skipit <- c("Quaternary", "Holocene", "Late Pleistocene")
      } else {
        skipit <- c("Holocene", "Late Pleistocene")
      }
      # add deep timescale
      if (length(geo_units) == 1) {
        p <- p + deeptime::coord_geo(
          dat  = geo_units,
          pos  = lapply(seq_len(length(geo_units)), function(x)
            "bottom"),
          size = lapply(seq_len(length(geo_units)), function(x)
            tip_labels_size),
          xlim = c(-tree_height, tree_height /
                     2),
          ylim = c(-tick_height * 5, ntips *
                     1.1),
          height = grid::unit(4, "line"),
          skip = skipit,
          abbrv = FALSE,
          rot = 90,
          center_end_labels = TRUE,
          bord = c("right", "top", "bottom"),
          neg  = TRUE
        )
      } else if (length(geo_units) == 2) {
        p <- p + deeptime::coord_geo(
          dat  = geo_units,
          pos  = lapply(seq_len(length(geo_units)), function(x)
            "bottom"),
          size = lapply(seq_len(length(geo_units)), function(x)
            tip_labels_size),
          xlim = c(-tree_height, tree_height /
                     2),
          ylim = c(-tick_height * 5, ntips *
                     1.1),
          skip = skipit,
          center_end_labels = TRUE,
          bord = c("right", "top", "bottom"),
          neg  = TRUE
        )

      }
    }
    #add axis title
    p <- p + ggplot2::scale_x_continuous(name = "Age (Ma)",
                                         limits = c(-tree_height, tree_height /
                                                      2))
    p <- ggtree::revts(p)
    # add ma ticks and labels
    xline <- pretty(c(0, max_age))[pretty(c(0, max_age)) < max_age]
    df <-
      data.frame(
        x = -xline,
        y = rep(-tick_height * 5, length(xline)),
        vx = -xline,
        vy = rep(-tick_height * 5 + tick_height, length(xline))
      )

    p <-
      p + ggplot2::geom_segment(ggplot2::aes(
        x = 0,
        y = -tick_height * 5,
        xend = -max_age,
        yend = -tick_height * 5
      )) +
      ggplot2::geom_segment(data = df, ggplot2::aes(
        x = x,
        y = y,
        xend = vx,
        yend = vy
      )) +
      ggplot2::annotate(
        "text",
        x = -rev(xline),
        y = -tick_height * 5 + tick_height * 2,
        label = rev(xline),
        size = tip_labels_size
      )

    # add vertical gray bars
    if (time_bars) {
      if (geo) {
        if ("epochs" %in% geo_units) {
          x_pos <- -rev(c(0, deeptime::get_scale_data("epochs")$max_age))
        } else {
          x_pos <-  -rev(c(0, deeptime::get_scale_data("periods")$max_age))
        }
      } else if (!geo) {
        x_pos <- -rev(xline)
      }
      for (k in 2:(length(x_pos))) {
        box_col <- "gray92"
        if (k %% 2 == 1)
          box_col <- "white"
        box <-
          ggplot2::geom_rect(
            xmin = x_pos[k - 1],
            xmax = x_pos[k],
            ymin = -tick_height * 5,
            ymax = ntips,
            fill = box_col
          )
        p <- gginnards::append_layers(p, box, position = "bottom")
      }
    }
    if (tip_labels) {
      # recenter legend
      tot <- max_age + tree_height / 2
      p <-
        p + ggplot2::theme(axis.title.x =
                             ggplot2::element_text(hjust = max_age /
                                                                  (2 * tot)))
    }
  }

  # add tip labels
  if (tip_labels == TRUE) {
    if (tip_labels_italics == TRUE) {
      p <-
        p + ggtree::geom_tiplab(
          ggplot2::aes(label = paste0('italic(`', label, '`)')),
          size = tip_labels_size,
          offset = tip_labels_offset,
          parse = TRUE
        )
    } 
    else if (tip_labels_formatted == TRUE ) {
      p <- p + ggtree::geom_tiplab(
        ggplot2::aes(label = label),
        size = tip_labels_size,
        offset = tip_labels_offset,
        parse = TRUE
      )
    } else {
      p <-
        p + ggtree::geom_tiplab(size = tip_labels_size,
                                offset = tip_labels_offset)
    }
  }

  # set up guides
  if (cladogenetic == TRUE) {
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(end_state_1),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(end_state_2),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(end_state_3),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)

    p <-
      p + ggtree::geom_tippoint(ggtree::aes(colour = factor(end_state_1),
                                            size = 0),
                                na.rm = TRUE,
                                alpha = 0.0)
    p <-
      p + ggtree::geom_tippoint(ggtree::aes(colour = factor(end_state_2),
                                            size = 0),
                                na.rm = TRUE,
                                alpha = 0.0)
    p <-
      p + ggtree::geom_tippoint(ggtree::aes(colour = factor(end_state_3),
                                            size = 0),
                                na.rm = TRUE,
                                alpha = 0.0)

    if ("other" %in% state_labels) {
      p <-
        p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(end_state_other),
                                               size = 0),
                                   na.rm = TRUE,
                                   alpha = 0.0)
    }
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(start_state_1),
                                             size =  0),
                                 na.rm = TRUE,
                                 alpha = 0.0)
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(start_state_2),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(start_state_3),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)

  } else if (cladogenetic == FALSE &
             "anc_state_1" %in% colnames(t@data)) {
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(anc_state_1),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(anc_state_2),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(anc_state_3),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)

    p <-
      p + ggtree::geom_tippoint(ggtree::aes(colour = factor(anc_state_1),
                                            size = 0),
                                na.rm = TRUE,
                                alpha = 0.0)
    p <-
      p + ggtree::geom_tippoint(ggtree::aes(colour = factor(anc_state_2),
                                            size = 0),
                                na.rm = TRUE,
                                alpha = 0.0)
    p <-
      p + ggtree::geom_tippoint(ggtree::aes(colour = factor(anc_state_3),
                                            size = 0),
                                na.rm = TRUE,
                                alpha = 0.0)

    if ("other" %in% state_labels) {
      p <-
        p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(anc_state_other),
                                               size = 0),
                                   na.rm = TRUE,
                                   alpha = 0.0)
    }

  } else {
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(end_state_1),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(end_state_2),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)
    p <-
      p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(end_state_3),
                                             size = 0),
                                 na.rm = TRUE,
                                 alpha = 0.0)

    p <-
      p + ggtree::geom_tippoint(ggtree::aes(colour = factor(end_state_1),
                                            size = 0),
                                na.rm = TRUE,
                                alpha = 0.0)
    p <-
      p + ggtree::geom_tippoint(ggtree::aes(colour = factor(end_state_2),
                                            size = 0),
                                na.rm = TRUE,
                                alpha = 0.0)
    p <-
      p + ggtree::geom_tippoint(ggtree::aes(colour = factor(end_state_3),
                                            size = 0),
                                na.rm = TRUE,
                                alpha = 0.0)

    if ("other" %in% state_labels) {
      p <-
        p + ggtree::geom_nodepoint(ggtree::aes(colour = factor(end_state_other),
                                               size = 0),
                                   na.rm = TRUE,
                                   alpha = 0.0)
    }
  }

  if (is.null(names(colors))) {
    breaks <- levels(state_labels)
  } else {breaks <- names(colors)}

  p <-
    p + ggplot2::scale_color_manual(values = colors, breaks = breaks)
  p <-
    p + ggplot2::guides(colour =
                          ggplot2::guide_legend("State",
                                                override.aes =
                                                  list(size = 4, alpha = 1.0)),
                        order = 1)
  p <- p + ggplot2::guides(size = "none")

  # plot pies at nodes (and shoulders)
  if (cladogenetic == TRUE) {

        # create state matrices (matrix of nodes (rows) and all
    # possible states (columns), values are pp. )
    state_probs <-
      .build_state_probs(t, state_labels, include_start_states = TRUE)
    dat_state_end <- state_probs$end
    dat_state_start <- state_probs$start

    # make pie plots
    pies_start <-
     .nodepie(
        dat_state_start,
        cols = 1:(ncol(dat_state_start) - 1),
        color = colors,
        alpha = state_transparency
      ) 
    pies_end <-
     .nodepie(
        dat_state_end,
        cols = 1:(ncol(dat_state_end) - 1),
        color = colors,
        alpha = state_transparency
      )

    # change 0s to avoid dividing by zero when calculating coordinates
    zeros <- which(dplyr::pull(p$data, "x") == 0)
    p$data[zeros, "x"] <- 0.0001

    # convert pie plots to lists

    # NODE PIES
    # save pies as images and plot as raster grobs
    pies_end_to_plot <- pies_end[node_idx]
    results_end <- list()
    for (i in seq_len(length(pies_end_to_plot))) {
      ggplot2::ggsave(
        paste0(tmp,"/.temp.png"),
        plot = pies_end_to_plot[[i]],
        bg = "transparent",
        width = 3,
        height = 3,
        units = "cm",
        dpi = 200
      )
      pie <- png::readPNG(paste0(tmp,"/.temp.png"))
      results_end[[i]] <-
        ggplotify::as.ggplot(grid::rasterGrob(pie, interpolate = TRUE))
    }
    # plotting data
    df_pies_end <- p$data[p$data$isTip == FALSE, ]
    # adjust nudges
    df_pies_end$x <- df_pies_end$x - node_pie_nudge_x
    df_pies_end$y <- df_pies_end$y - node_pie_nudge_y

    # SHOULDER PIES
    # save pies as images and plot as raster grobs
    results_start <- list()
    for (i in seq_len(length(pies_start))) {
      ggplot2::ggsave(
        paste0(tmp,"/.temp.png"),
        plot = pies_start[[i]],
        bg = "transparent",
        width = 3,
        height = 3,
        units = "cm",
        dpi = 200
      )
      pie <- png::readPNG(paste0(tmp,"/.temp.png"))
      results_start[[i]] <-
        ggplotify::as.ggplot(grid::rasterGrob(pie, interpolate = TRUE))
    }
    # plotting data
    df_pies_start <- p$data
    df_pies_start$x <- df_pies_start$x[match(df_pies_start$parent,
                                             df_pies_start$node)]
    # adjust nudges
    df_pies_start$x <- df_pies_start$x - shoulder_pie_nudge_x
    df_pies_start$y <- df_pies_start$y - shoulder_pie_nudge_y

    # save pies as images and plot as raster grobs
    # TIP PIES
    if (tip_pies == TRUE) {
      pies_tip <- pies_end[tip_idx]
      results_tip <- list()
      for (i in seq_len(length(pies_tip))) {
        ggplot2::ggsave(
          paste0(tmp,"/.temp.png"),
          plot = pies_end[[i]],
          bg = "transparent",
          width = 3,
          height = 3,
          units = "cm",
          dpi = 200
        )
        pie <- png::readPNG(paste0(tmp,"/.temp.png"))
        results_tip[[i]] <-
          ggplotify::as.ggplot(grid::rasterGrob(pie, interpolate = TRUE))
      }
      # plotting data
      df_pies_tip <- p$data[p$data$isTip == TRUE, ]
      # adjust nudges
      df_pies_tip$x <- df_pies_tip$x - tip_pie_nudge_x
      df_pies_tip$y <- df_pies_tip$y - tip_pie_nudge_y
    }

    if (tip_pies == TRUE) {
      df_pies <- rbind(df_pies_end, df_pies_start, df_pies_tip)
      results <- c(results_end, results_start, results_tip)
      sizes <- c(rep(node_pie_size, nrow(df_pies_end)),
                 rep(shoulder_pie_size, nrow(df_pies_start)),
                 rep(tip_pie_size, nrow(df_pies_tip)))
    } else {
      df_pies <- rbind(df_pies_end, df_pies_start)
      results <- c(results_end, results_start)
      sizes <- c(rep(node_pie_size, nrow(df_pies_end)),
                 rep(shoulder_pie_size, nrow(df_pies_start)))
    }

    p <-
      p + ggpp::geom_plot(data = df_pies,
                          mapping = ggplot2::aes(
                            x = x,
                            y = y,
                            label = results
                          ),
                          vp.width = sizes,
                          vp.height = sizes,
                          hjust = 0.5,
                          vjust = 0.5
      )

  } else {
    # create state matrices (matrix of nodes (rows) and all
    # possible states (columns), values are pp. )
    dat_state_anc <-
      .build_state_probs(t, state_labels, include_start_states = FALSE)[[1]]
    if (sum(otherpp, na.rm = TRUE) == 0) {
      dat_state_anc$other <- NULL
    }
    pies_anc <-
      .nodepie(
        dat_state_anc,
        cols = 1:(ncol(dat_state_anc) - 1),
        color = colors,
        alpha = state_transparency
      )
    zeros <- which(dplyr::pull(p$data, "x") == 0)
    p$data[zeros, "x"] <- 0.0001

    # convert pie plots to lists

    # NODE PIES
    # save pies as images and plot as raster grobs
    pies_anc_to_plot <- pies_anc[node_idx]
    results_anc <- list()
    for (i in seq_len(length(pies_anc_to_plot))) {
      ggplot2::ggsave(
        paste0(tmp,"/.temp.png"),
        plot = pies_anc_to_plot[[i]],
        bg = "transparent",
        width = 3,
        height = 3,
        units = "cm",
        dpi = 200
      )
      pie <- png::readPNG(paste0(tmp,"/.temp.png"))
      results_anc[[i]] <-
        ggplotify::as.ggplot(grid::rasterGrob(pie, interpolate = TRUE))
    }
    # plotting data
    df_pies_anc <- p$data[p$data$isTip == FALSE, ]
    # adjust nudges
    df_pies_anc$x <- df_pies_anc$x - node_pie_nudge_x
    df_pies_anc$y <- df_pies_anc$y - node_pie_nudge_y

    # save pies as images and plot as raster grobs
    # TIP PIES
    if (tip_pies == TRUE) {
      pies_tip <- pies_anc[tip_idx]
      results_tip <- list()
      for (i in seq_len(length(pies_tip))) {
        ggplot2::ggsave(
          paste0(tmp,"/.temp.png"),
          plot = pies_tip[[i]],
          bg = "transparent",
          width = 3,
          height = 3,
          units = "cm",
          dpi = 200
        )
        pie <- png::readPNG(paste0(tmp,"/.temp.png"))
        results_tip[[i]] <-
          ggplotify::as.ggplot(grid::rasterGrob(pie, interpolate = TRUE))
      }
      # plotting data
      df_pies_tip <- p$data[p$data$isTip == TRUE, ]
      # adjust nudges
      df_pies_tip$x <- df_pies_tip$x - tip_pie_nudge_x
      df_pies_tip$y <- df_pies_tip$y - tip_pie_nudge_y
    }

    if (tip_pies == TRUE) {
      df_pies <- rbind(df_pies_anc, df_pies_tip)
      results <- c(results_anc, results_tip)
      sizes <- c(rep(node_pie_size, nrow(df_pies_anc)),
                 rep(tip_pie_size, nrow(df_pies_tip)))
    } else {
      df_pies <- df_pies_anc
      results <- results_anc
      sizes <- rep(node_pie_size, nrow(df_pies_anc))
    }

    p <-
      p + ggpp::geom_plot(data = df_pies,
                          mapping = ggplot2::aes(
                            x = x,
                            y = y,
                            label = results
                          ),
                          vp.width = sizes,
                          vp.height = sizes,
                          hjust = 0.5,
                          vjust = 0.5
      )
  }

  # add node labels (text)
  if (is.null(node_labels_as) == FALSE) {
    # add clado plotting data for node labels
    if (cladogenetic == TRUE) {
      x <- ggtree::fortify(tree)$x
      y <- ggtree::fortify(tree)$y
      x_anc <- numeric(n_node)
      node_index <- numeric(n_node)
      for (i in 1:n_node) {
        if (.getParent(tree, i) != 0) {
          # if not the root, get the x coordinate for the parent node
          x_anc[i] <- x[.getParent(tree, i)]
          node_index[i] <- i
        }
      }
      shoulder_data <-
        data.frame(node = node_index,
                   x_anc = x_anc,
                   y = y)
      if (timeline == TRUE) {
        shoulder_data$x_anc <- shoulder_data$x_anc - tree_height
      }
      `%<+%` <- ggtree::`%<+%`
      p <- p %<+% shoulder_data
    }

    if (node_labels_as == "state") {
      if (cladogenetic == TRUE) {
        p <-
          p + ggtree::geom_text2(
            ggplot2::aes(label = end_state_1, subset = !isTip),
            hjust = 1,
            nudge_x = node_labels_offset,
            size = node_labels_size
          ) +
          ggtree::geom_text(
            ggplot2::aes(
              label = start_state_1,
              x = x_anc,
              y = y
            ),
            hjust = 0,
            nudge_x = node_labels_offset,
            size = node_labels_size,
            na.rm = TRUE
          )
      } else if (cladogenetic == FALSE &
                 state_pos_str_base[1] == "anc_state_") {
        p <-
          p + ggtree::geom_text2(
            ggplot2::aes(label = anc_state_1, subset = !isTip),
            hjust = 1,
            nudge_x = node_labels_offset,
            size = node_labels_size
          )
      } else if (cladogenetic == FALSE &
                 state_pos_str_base != "anc_state_") {
        p <-
          p + ggtree::geom_text2(
            ggplot2::aes(label = end_state_1, subset = !isTip),
            hjust = 1,
            nudge_x = node_labels_offset,
            size = node_labels_size
          )
      }
    } else if (node_labels_as == "state_posterior") {
      if (cladogenetic == TRUE) {
        p <-
          p + ggtree::geom_text2(
            ggplot2::aes(
              label = .convertAndRound(end_state_1_pp),
              subset = !isTip
            ),
            hjust = 1,
            nudge_x = node_labels_offset,
            size = node_labels_size
          ) +
          ggtree::geom_text(
            ggplot2::aes(
              label = .convertAndRound(start_state_1_pp),
              x = x_anc,
              y = y
            ),
            hjust = 0,
            nudge_x = node_labels_offset,
            size = node_labels_size,
            na.rm = TRUE
          )
      } else if (cladogenetic == FALSE &
                 state_pos_str_base[1] == "anc_state_") {
        p <-
          p + ggtree::geom_text2(
            ggplot2::aes(
              label = .convertAndRound(anc_state_1_pp),
              subset = !isTip
            ),
            hjust = 1,
            nudge_x = node_labels_offset,
            size = node_labels_size
          )
      } else if (cladogenetic == FALSE &
                 state_pos_str_base != "anc_state_") {
        p <-
          p + ggtree::geom_text2(
            ggplot2::aes(
              label = .convertAndRound(end_state_1_pp),
              subset = !isTip
            ),
            hjust = 1,
            nudge_x = node_labels_offset,
            size = node_labels_size
          )
      }
    } else if (node_labels_as == "node_posterior") {
      p <-
        p + ggtree::geom_nodelab(
          ggplot2::aes(label = .convertAndRound(posterior)),
          hjust = 1,
          nudge_x = node_labels_offset,
          size = node_labels_size
        )
    }
  }

  # add tip states labels (text)
  if (tip_labels_states == TRUE) {
    if (state_pos_str_base[1] == "anc_state_") {
      p <- p + ggtree::geom_tiplab(
        ggplot2::aes(label = anc_state_1),
        hjust = "center",
        offset = tip_labels_states_offset,
        size = tip_labels_states_size
      )
    } else {
      p <- p + ggtree::geom_tiplab(
        ggplot2::aes(label = end_state_1),
        hjust = "center",
        offset = tip_labels_states_offset,
        size = tip_labels_states_size
      )
    }
  }

  # add space on x axis for tip labels
  if (tip_labels == TRUE & timeline == FALSE) {
    p <- p + ggtree::xlim(0, tree_height + tree_height / 2)
  }

  # clean up pngs
  unlink(paste0(tmp,"/.temp.png"))

  return(p)
}

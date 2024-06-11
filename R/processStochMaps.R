#' processStochMaps
#' 
#' @param tree (treedata object; none) Output of readTrees() function
#' containing tree.
#' @param paths (vector of character strings; no default) File path(s) to
#' stochastic map trace(s).
#' @param simmap (multiphylo; none) A multiphylo object with simmaps in 
#' phytools format.
#' @param states (vector of character strings; no default) The character
#' states.
#' @param num_intervals (numeric; default 1001) The number of intervals
#' to divide the tree into.
#' @param verbose (logical; default TRUE) Print status of processing on screen.
#' 
#' @param ... (various) Additional arguments passed to readTrace()
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
#' }
#' 
#' @export
processStochMaps <- function(tree,
                             paths = NULL,
                             simmap = NULL,
                             states,
                             num_intervals = 1000,
                             verbose = TRUE,
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

    # compute the number of states
    nstates <- length(states)
    
    # create the index map
    map <- matchNodes(tree@phylo)
    
    # either paths or simmap must be provided
    if ( !is.null(paths) ) { # samples in files
        
        # read traces
        samples <- readTrace(paths, verbose = verbose, ...)

        # combine multiple samples together
        if ( length(samples) > 1 ) {
            samples <- combineTraces(samples)
            samples <- samples[[1]]
        } else {
            samples <- samples[[1]]
        }
        
        # compute the number of samples
        nsamples <- nrow(samples)
    
    } else if ( !is.null(simmap) ) { # samples in phytools format
      
        message("Reformatting simmap objects")
      
        # make the samples
        samples <- as.data.frame(do.call(rbind, lapply(simmap, function(map) {
            sapply(map$maps, function(edge) {
                edge <- rev(edge)
                return(paste0("{", paste0(paste0(names(edge),",", edge), collapse = ":"),"}"))
            })
        })))
        
        # add a root edge
        root_edge_samples <- sapply(simmap, function(map) {
            return(paste0("{", tail(names(map$maps[[1]]), n = 1), ",0}"))
        })
        samples <- cbind(samples, root_edge_samples)
        
        # get the nodes
        nodes <- c(tree@phylo$edge[,2], ape::Ntip(tree@phylo) + 1)
        colnames(samples) <- map$Rev[nodes]
        
        # compute the number of samples
        nsamples <- length(simmap)
        
    } else {
        stop("Please provide either a paths or simmap argument.")
    }
    
    message("Processing stochastic maps")
    
    # get the number of branches
    # including the root branch
    num_branches <- length(tree@phylo$edge.length) + 1
    root_index   <- ape::Ntip(tree@phylo) + 1
    
    # get the dt
    root_age <- max(ape::branching.times(tree@phylo))
    if (!is.null(tree@phylo$root.edge)) {
        root_age <- root_age + tree@phylo$root.edge
    } else {
        tree@phylo$root.edge <- 0
    }
    dt <- root_age / num_intervals
    
    # loop over branches
    dfs <- vector("list", num_branches)
    
    if (verbose) { pb <- txtProgressBar(min = 0, max = num_branches, initial = 0) }
    
    for(i in 1:num_branches) {
      
        # get the branch indexes
        R_index   <- map$R[i]
        Rev_index <- as.character(map[R_index,2])
        
        # get the time points
        if ( R_index == root_index ) {
            this_edge_length <- tree@phylo$root.edge
        } else {
            this_edge_length <- tree@phylo$edge.length[tree@phylo$edge[,2] == R_index]
        }
        these_pts <- seq(0, this_edge_length, by = dt)
        
        # get the samples
        branch_samples <- samples[,Rev_index]
        branch_samples <- gsub("{", "", branch_samples, fixed = TRUE)
        branch_samples <- gsub("}", "", branch_samples, fixed = TRUE)
        
        # split the per event
        branch_samples <- strsplit(branch_samples, ":")
        
        # get the times per state
        branch_samples <- lapply(branch_samples, function(sample) {
            sample <- do.call(rbind, strsplit(sample, ","))
            sample_states <- sample[,1]
            sample_times  <- as.numeric(sample[,2])
            names(sample_times) <- sample_states
            # sample_times <- rev(sample_times) # turn this on for plotting the output from (old) tensorphylo runs
            return(cumsum(sample_times))
        })
        
        # get the state per interval
        if ( this_edge_length == 0 ) {
            branch_states_per_interval <- t(t(match(names(unlist(branch_samples)), states)))
        } else {
            branch_states_per_interval <- do.call(rbind, lapply(branch_samples, function(sample) {
                match(names(sample)[findInterval(these_pts, sample) + 1], states)
            }))
        }
        
        # compute probability of each state per interval
        branch_prob_per_state <- apply(branch_states_per_interval, 2, tabulate, nbins = nstates) / nsamples
        rownames(branch_prob_per_state) <- states
        
        # now do the vertical segments
        vert_prob_per_state <- t(branch_prob_per_state[,ncol(branch_prob_per_state), drop = FALSE])
        
        # make the df
        this_df <- data.frame(index = Rev_index, bl = this_edge_length, x0 = these_pts, x1 = c(these_pts[-1], this_edge_length), vert = FALSE)
        this_df <- cbind(this_df, t(branch_prob_per_state))
        vert_df <- cbind(data.frame(index = Rev_index, bl = this_edge_length, x0 = this_edge_length, x1 = this_edge_length, vert = TRUE), vert_prob_per_state)
        this_df <- rbind(this_df, vert_df)        
        
        # store
        dfs[[i]] <- this_df
        
        if (verbose) { setTxtProgressBar(pb,i) }
        
    }
    if (verbose) { close(pb) }

    # combine the branches
    dfs <- do.call(rbind, dfs)
    
    # get node instead of index 
    # node is R's standard numbering for nodes
    # index is RevBayes specific 
    colnames(map) <- c("node", "index")
    map$index  <- as.character(map$index)
    dfs <- dplyr::full_join(map,dfs, by = "index")
    dfs$index <- NULL
    
    return(dfs)
    
}






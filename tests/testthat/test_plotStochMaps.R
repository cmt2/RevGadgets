context("tests the plotStochMaps function")

test_that("plots stochastic maps", {
  # get files
  treefile <- system.file("extdata",
                          "stoch_map_test_tmp/tree.nexus",
                          package="RevGadgets")
  mapsfile <- system.file("extdata",
                          "stoch_map_test_tmp/maps.log",
                          package="RevGadgets")
  plot_file <-
    system.file("extdata", 
                "graphs/plotStochMaps_df.rds", 
                package = "RevGadgets")
  
  
  # read in and process data 
  tree <- readTrees(treefile)[[1]][[1]]
  stoch_map_df <- processStochMaps(tree,
                                   mapsfile, 
                                   states = as.character(0:4), 
                                   burnin = 0.9)
  
  # new plot 
  plot_new <- plotStochMaps(tree = tree,
                            maps = stoch_map_df,
                            color_by = "MAP",
                            colors = "default",
                            tree_layout = "rectangular",
                            tip_labels = FALSE)
  
  # read original plot data object
  plot_orig <- readRDS(plot_file)
  
  tmp <- tempdir()
  pdf(paste0(tmp,"/Rplots.pdf"))
  # test for errors in plot_new
  expect_error(print(plot_new), NA)
  dev.off()
  
  # compare plot data objects
  expect_equal(plot_new$data, plot_orig)
  
})

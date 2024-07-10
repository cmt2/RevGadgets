context("tests the processStochMaps function")

test_that("processes stochastic map logs", {
  # read in and process file
  treefile <- system.file("extdata",
                          "stoch_map_test_tmp/tree.nexus",
                          package="RevGadgets")
  
  tree <- readTrees(treefile)[[1]][[1]]
  
  # process samples
  mapsfile <- system.file("extdata",
                          "stoch_map_test_tmp/maps.log",
                          package="RevGadgets")
  
  stoch_map_df <- processStochMaps(tree,
                                   mapsfile, 
                                   states = as.character(0:4), 
                                   burnin = 0.9)
  # test file format
  expect_equal(class(stoch_map_df), "data.frame")
  
  # check data dimensions
  expect_equal(dim(stoch_map_df), c(6119, 10))
  
  # check column names
  expect_equal(colnames(stoch_map_df), 
               c("node", "bl", "x0", "x1", "vert",
                 "0", "1", "2", "3", "4"))
  
})

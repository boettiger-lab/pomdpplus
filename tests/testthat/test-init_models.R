library(appl)
testthat::context("init_models")

testthat::test_that("init models generate the set of alpha vectors and actions", {
  
  source(system.file("examples/example_init_models.R", package = "pomdpplus"))
  x <- init_models(T,O,R,GAMMA,Num_model,initial)
  
  
  testthat::expect_is(x, "list")
  

})

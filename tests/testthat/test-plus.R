testthat::context("plus")

testthat::test_that("plus generates the results of the forward simulation", {
  
  source(system.file("examples/example_plus.R", package = "pomdpplus"))
  devtools::load_all()
  x <- plus(input,t,Num_sim,n_true,n_sample,initial,P)

  
  testthat::expect_is(x, "list")
  
 
})

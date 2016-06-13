testthat::context("run_sim")

testthat::test_that("run_sim generates the outputs of the forward simulations", {
  
  source(system.file("examples/example_run_sim.R", package = "pomdpplus"))
  x <- run_sim(T,O,R,GAMMA,av,aa,n_true,Num_sim,t,Num_model,initial,n_sample, P)
  
  
  testthat::expect_is(x, "list")
  
  
})

testthat::context("run_sim")

testthat::test_that("run_sim generates the outputs of the forward simulations", {
  
  source(system.file("examples/example_run_sim.R", package = "pomdpplus"))
  devtools::load_all()
  out <- run_sim(T,O,R,GAMMA,av,aa,n,Num_sim,t,N,init,n_sample, P)
  
  
  testthat::expect_output(str(out), "list of 7")
  
  
})

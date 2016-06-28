testthat::context("plus_seq")


# creating a simple example for testing
theta = seq(0, 1, by = 0.2)
Num_s = 2; Num_z = 2; Num_a = 2;
GAMMA = 0.95


initt <- function(theta,Num_s,Num_a,Num_z){
  transit = array(0, dim = c(Num_s,Num_s,Num_a))
  emis = array(0, dim = c(Num_s,Num_z,Num_a))
  transit[,,1] = matrix(c(1-theta,theta,0,1), 2, 2, byrow= TRUE)
  transit[,,2] = matrix(c(1,0,1-theta,theta), 2, 2, byrow= TRUE)
  emis[,,1] = matrix(c(1-theta,theta,theta,1-theta), 2, 2, byrow= TRUE)
  emis[,,2] = emis[,,1]
  output <- list(transit,emis)
}

T = vector("list", length = length(theta))
O = vector("list", length = length(theta))

for(i in 1:length(theta)){
  out <- initt(theta[i],Num_s,Num_a,Num_z)
  T[[i]] = out[[1]]
  O[[i]] = out[[2]]
}

R = matrix(c(0,-1,-10,-11),2,2,byrow = TRUE)
Num_model = length(T)
seq_act = c(1,1,1,2,1,1,1,2,2,1)
seq_obs = c(1,1,2,1,1,1,2,2,1,1)
seq = vector("list", length = 2)
seq[[1]] = seq_obs; seq[[2]] = seq_act
Num_sim = 10
n_sample = 3
initial = array(1, dim = Num_s) / Num_s
P = (array(1,dim = length(T))/ length(T))

input = vector("list", length = 4)
input[[1]] = T; input[[2]] = O; input[[3]] = R; input[[4]] = GAMMA

testthat::test_that("plus_seq generates the results of the forward simulation", {
  
  #  source(system.file("examples/example_plus.R", package = "pomdpplus"))
  x <- plus_seq(input,Num_sim,seq,n_sample,initial,P)
  
  testthat::expect_is(x, "list")
  
  
})

POMDP Planning and Learning in Uncertain Systems
================
Carl Boettiger and Milad Memarzadeh

[![Build Status](https://drone.carlboettiger.info/api/badges/boettiger-lab/pomdpplus/status.svg)](https://drone.carlboettiger.info/boettiger-lab/pomdpplus)

<!-- README.md is generated from README.Rmd. Please edit that file -->
``` r
library("pomdpplus")
```

Initialize a simple POMDP model:

``` r
theta = seq(0, 1, by = 0.2)
Num_s = 2 # number of states
Num_z = 2 # number of observations 
Num_a = 2 # number of actions
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

t = 10
Num_sim = 10
n_true = 4   # 
n_sample = 3

initial = array(1, dim = Num_s) / Num_s
P = (array(1,dim = length(T))/ length(T))
```

Run model intialization

``` r
alphas <- init_models(T, O, R, GAMMA, initial = initial)
```

Run PLUS:

``` r
plus_results <- plus(list(T, O, R, GAMMA),t, Num_sim, n_true, n_sample = 3, initial, P)
```

Run simulation:

``` r
sim_results <- run_sim(T, O, R, GAMMA, alphas[["av"]], alphas[["aa"]], n_true, Num_sim, t, Num_model, initial, n_sample, P)
```

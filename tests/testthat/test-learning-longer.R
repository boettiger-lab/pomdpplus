library("appl")
library("pomdpplus")

## Problem definition
states <- 0:20
actions <- states
obs <- states
reward_fn <- function(x,h) pmin(x,h)

## parameters we will learn
vars <- expand.grid(K = seq(8, 16, by = 2))

## Bind this to a data.frame listing each of the fixed parameters across all runs
fixed <- data.frame( r = 1, sigma_g = 0.1, sigma_m = 0.1, discount = 0.95, precision = 0.1)
pars <- data.frame(vars, fixed)

## Create the models
models <- lapply(1:dim(pars)[1], function(i){
  f <- appl:::ricker(pars[i, "r"], pars[i, "K"])
  appl::fisheries_matrices(states, actions, obs,
                           reward_fn, f = f,
                           sigma_g = pars[i, "sigma_g"],
                           sigma_m  = pars[i, "sigma_m"])
})

alphas <- sarsop_plus(models,
                      discount = pars[1, "discount"],
                      precision = pars[1, "precision"],
                      log_data = pars)


unif <- compute_plus_policy(alphas, models)
testthat::expect_is(unif, "data.frame")


## library(tidyverse)
## ggplot(unif, aes(state, policy)) + geom_line()

true_i <- 2
out <- sim_plus(models = models, discount = discount,
                x0 = 5, a0 = 1, Tmax = 10,
                true_model = models[[true_i]],
                alphas = alphas)

testthat::test_that("plus prefers the true model after learning period", {
  testthat::expect_gt(out$model_posterior[10,true_i], out$model_posterior[10,1])
})


true_i <- 3
out <- sim_plus(models = models, discount = discount,
                x0 = 5, a0 = 1, Tmax = 10,
                true_model = models[[true_i]],
                alphas = alphas)
testthat::test_that("plus prefers the true model after learning period", {
  testthat::expect_gt(out$model_posterior[10,true_i], out$model_posterior[10,1])
})


true_i <- 4
out <- sim_plus(models = models, discount = discount,
                x0 = 5, a0 = 1, Tmax = 10,
                true_model = models[[true_i]],
                alphas = alphas)
testthat::test_that("plus prefers the true model after learning period", {
  testthat::expect_gt(out$model_posterior[10,true_i], out$model_posterior[10,1])
})



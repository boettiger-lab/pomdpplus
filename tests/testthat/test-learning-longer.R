library("sarsop")
library("pomdpplus")
set.seed(123)
## Problem definition
states <- seq(0,1, length.out = 20)
actions <- states
obs <- states
reward_fn <- function(x,h) pmin(x,h)
discount = 0.95

## parameters we will learn
vars <- expand.grid(K = seq(.2, .8, length = 4))
## Bind this to a data.frame listing each of the fixed parameters across all runs
fixed <- data.frame( r = 1, sigma_g = 0.2, sigma_m = 0.2, timeout = 100)
pars <- data.frame(vars, fixed)

## Create the models
models <- lapply(1:dim(pars)[1], function(i){
  f <- sarsop:::ricker(pars[i, "r"], pars[i, "K"])
  fisheries_matrices(states, actions, obs,
                           reward_fn, f = f,
                           sigma_g = pars[i, "sigma_g"],
                           sigma_m  = pars[i, "sigma_m"])
})

## consider logging and saving these for reference in tests
alphas <- sarsop_plus(models,
                      discount = discount,
                      timeout = pars[1, "timeout"],
                      log_data = pars, mc.cores = parallel::detectCores())


unif <- compute_plus_policy(alphas, models)
testthat::expect_is(unif, "data.frame")
## library(tidyverse)
## ggplot(unif, aes(state, policy)) + geom_line()


true_i <- 1
Tmax <- 20
out <- sim_plus(models = models, discount = discount,
                x0 = 5, a0 = 1, Tmax = Tmax,
                true_model = models[[true_i]],
                alphas = alphas)

testthat::test_that("plus prefers the true model after learning period", {
  testthat::expect_equal(which.max(out$model_posterior[Tmax,]), true_i)
})

true_i <- 2
out <- sim_plus(models = models, discount = discount,
                x0 = 5, a0 = 1, Tmax = Tmax,
                true_model = models[[true_i]],
                alphas = alphas)
testthat::test_that("plus prefers the true model after learning period", {
  testthat::expect_equal(which.max(out$model_posterior[Tmax,]), true_i)
})


true_i <- 3
out <- sim_plus(models = models, discount = discount,
                x0 = 5, a0 = 1, Tmax = Tmax,
                true_model = models[[true_i]],
                alphas = alphas)
testthat::test_that("plus prefers the true model after learning period", {
  testthat::expect_equal(which.max(out$model_posterior[Tmax,]), true_i)
})

set.seed(123)
mine <- sim_plus(models = models, discount = discount,
                x0 = 5, a0 = 1, Tmax = Tmax,
                true_model = models[[true_i]],
                alphas = alphas)

load("test.rda")
milad <- mine
testthat::expect_identical(mine, milad)

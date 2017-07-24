library("sarsop")
library("pomdpplus")
## Problem definition
states <- 0:20
actions <- states
obs <- states
sigma_g <- 0.1
sigma_m <- sigma_g
reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95

## Using two candidate models
r <- 0.5
K <- c(10,15)
K1 <- function(x, h){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K[1]) )
}
K2 <- function(x, h){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K[2]) )
}
models <- lapply(list(K1,K2), function(f) appl::fisheries_matrices(states, actions, obs, reward_fn, f, sigma_g, sigma_m))


alphas <- sarsop_plus(models, discount, precision = .1)
unif <- compute_plus_policy(alphas, models)
testthat::expect_is(unif, "data.frame")

out <- sim_plus(models = models, discount = discount,
                x0 = 5, a0 = 1, Tmax = 20,
                true_model = models[[2]],
                alphas = alphas)

testthat::test_that("plus prefers the true model after learning period", {
  testthat::expect_gt(out$model_posterior[10,2], out$model_posterior[10,1])
})




## Check logging works
log <- tempdir()
## make sure log is empty first
lapply(list.files(log), function(x) unlink(paste(log, x, sep = "/")))

log_data <- data.frame(model = "ricker", r = r, K = K, C = NA, sigma_g = sigma_g, sigma_m = sigma_m)

alphas <- sarsop_plus(models, discount, precision = 1,
                      log_dir = log, log_data = log_data)


meta <- meta_from_log(parameters = data.frame(model = "ricker", r = r), log_dir = log)

## Make sure we have only two matches
testthat::expect_length(meta[,1], 2)

log_alphas <- alphas_from_log(meta, log_dir = log)
log_models <- models_from_log(meta)

testthat::expect_identical(alphas, log_alphas)
testthat::expect_identical(models, log_models)

log_fs <- f_from_log(meta)

lapply(list.files(log), function(x) unlink(paste(log, x, sep = "/")))


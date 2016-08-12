states <- 0:20
actions <- states
obs <- states
sigma_g <- 0.1
sigma_m <- sigma_g
reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95
K1 <- function(x, h, r = 1, K = 15){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}
K2 <- function(x, h, r = 1, K = 10){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}
models <- lapply(list(K1, K2), function(f) appl::fisheries_matrices(states, actions, obs, reward_fn, f, sigma_g, sigma_m))


alphas <- sarsop_plus(models, discount, precision = 1)
unif <- compute_plus_policy(alphas, models)
testthat::expect_is(unif, "data.frame")

out <- sim_plus(models = models, discount = discount,
                x0 = 5, a0 = 1, Tmax = 10,
                true_model = models[[2]],
                alphas = alphas)





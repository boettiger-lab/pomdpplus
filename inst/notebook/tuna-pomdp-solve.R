library("appl")
library("pomdpplus")

mc.cores <- round(parallel::detectCores() / 4)

log_dir <- "tuna-100"
states <- seq(0, 1.2, len=100)
actions <- states
obs <- states
discount <- 0.99
reward_fn <- function(x,h) pmin(x,h)

## MLE parameter estimates
K <- 0.9903371
r <- 0.05699246
sigma_g <- 0.01720091


## parameters we will learn
vars <- expand.grid(r = rev(seq(0.025, 0.2, by =0.025)), sigma_m = c(0.1, 0.3, 0.5))


## Bind this to a data.frame listing each of the fixed parameters across all runs
fixed <- data.frame( K = K, C = NA, sigma_g = sigma_g, discount = discount, model = "ricker", 
                     precision = 0.0000001, timeout = 50000, timeInterval = 5000,
                     max_state = max(states), max_obs = max(obs), max_action = max(actions), 
                     min_state = min(states), min_obs = min(obs), min_action = min(actions))
pars <- data.frame(vars, fixed)


## Create the models

models <- lapply(1:dim(pars)[1], function(i){
  f <- switch(as.character(pars[i, "model"]), 
              allen = appl:::allen(pars[i, "r"], pars[i, "K"], pars[i, "C"]),
              ricker = appl:::ricker(pars[i, "r"], pars[i, "K"])
  )
  appl::fisheries_matrices(states, actions, obs, 
                           reward_fn, f = f, 
                           sigma_g = pars[i, "sigma_g"], 
                           sigma_m  = pars[i, "sigma_m"])
})



## Now for the very slow step: compute alphas for the above examples


alphas <- sarsop_plus(models, 
                      discount = pars[1, "discount"], 
                      precision = pars[1, "precision"], 
                      timeout = pars[1, "timeout"],
                      timeInterval = pars[1, "timeInterval"],
                      log_dir = log_dir, 
                      log_data = pars,
                      mc.cores = mc.cores)



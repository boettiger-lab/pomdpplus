## Assumes through-out that observation does not depend on action, and thus policy is determiend by observation alone and not observation + previous action

pomdp_planning <- function(models, discount, model_prior, verbose = TRUE, mc.cores = 1L, ...){
  Qs <- init_models(models, discount, verbose, mc.cores, ...)
  compute_policy(Qs, model_prior)
}

get_Q <- function(alpha, alpha_action, n_a){
  n_x <- dim(alpha)[[1]]
  unname(as.data.frame(
    lapply(1:n_a, function(i){
      j <- which(alpha_action == i)[1]
      if(!is.na(j))
        alpha[, j]
      else
        rep(0, n_x)
    })
  ))
}


init_models <- function(models, discount, verbose = TRUE, mc.cores = 1L, ...){

  parallel::mclapply(models, function(m){

    ## Assume uniform prior belief over states if not given for the model
    if(is.null(m$initial)) initial <- rep(1, dim(m$observation)[[1]]) / dim(m$observation)[[1]]

    result <- run_pomdp(m$transition, m$observation, m$reward, discount, initial, verbose, ...)

    V <- t(t(result$alpha) %*% m$observation[,,1])  ## Note the assumption that observation is indep of action
    get_Q(V, result$alpha_action, dim(m$observation)[[3]])

  }, mc.cores = mc.cores)

}

compute_policy <- function(Qs, model_prior){

  ## Compute optimal policy based on alpha vectors, V(b) = max_i \sum_x b(x) alpha_i(x)
  EV <- do.call(`+`, lapply(1:length(Qs), function(i) as.matrix(Qs[[i]]) * model_prior[i]))
  value <- apply(EV, 1, max)
  policy <- apply(EV, 1, function(x) which.max(x))

  ## Note that policy is given as index numbers to the action vector, and state as index numbers to the states vector
  state <- 1:dim(Qs[[1]])[[1]] # For plotting convenience
  data.frame(policy, value, state)

}



pomdp_learning <- function(x0, models, model_prior = NULL, Tmax = 20,
                           true_transition, true_observation, true_utility = NULL){

  if(is.null(true_utility)) true_utility <- models[[1]]$reward

  ## Compute alpha vectors for all models (time-intensive step)
  Qs <- init_models(models, discount, models_prior, verbose, mc.cores, ...)

  ## Initialize
  value <- obs <- action <- state <- numeric(Tmax)
  posterior <- array(NA, dim = c(Tmax, length(models)))
  posterior[1,] <- model_prior

  n_s <- dim(models[[1]][["observation"]])[1]
  n_z <- dim(models[[1]][["observation"]])[2]
  state[1] <- x0


  ## Forward simulation, updating belief
  for(t in 1:Tmax){
    out <- compute_policy(Qs, posterior[t,])

    obs_prob <- true_observation[state[t], , 1]
    obs[t] <- sample(1:n_z, 1, prob = obs_prob)
    action[t] <- out$policy[obs[t]]
    value[t] <- true_utility[state[t], action[t]] * discount^(t-1)
    trans_prob <- true_transition[state[t], , action[t]]
    state[t+1] <- sample(1:n_s, 1, prob = trans_prob)

    q[t+1,] <- bayes_update_qt(q[t,], state[t], state[t+1], action[t], transition)


  }

  ## assemble data frame without dummy year for starting action
  df <- data.frame(time = 0:Tmax, state = state[1:(Tmax+1)], obs, action, value)
  df[2:(Tmax+1),]

  list(df = df, posterior = posterior)
}




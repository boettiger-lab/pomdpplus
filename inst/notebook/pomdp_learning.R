## Assumes through-out that observation does not depend on action, and thus policy is determiend by observation alone and not observation + previous action

pomdp_planning <- function(models, discount, model_prior, states_prior = NULL, verbose = TRUE, mc.cores = 1L, ...){
  alphas <- init_models(models = models, discount = discount, states_prior = states_prior, verbose = verbose, mc.cores = mc.cores, ...)
  compute_policy(alphas, model_prior, states_prior, models)
}



init_models <- function(models, discount, states_prior = NULL,
                          verbose = TRUE, mc.cores = 1L, ...){

  n_states <- dim(models[[1]]$transition)[[1]]
  n_actions <- dim(models[[1]]$transition)[[3]]

  if(is.null(states_prior))
    states_prior <- vapply(1:length(models), function(i) rep(1, n_states) / n_states, numeric(n_states))


  parallel::mclapply(1:length(models), function(i){

    belief <- states_prior[,i]
    result <- run_pomdp(m$transition, m$observation, m$reward, discount, belief, verbose, ...)

    regularize_alpha(result$alpha, result$alpha_action, n_actions)


  }, mc.cores = mc.cores)

}

compute_policy <- function(alphas, model_prior, states_prior){

  belief <- states_prior

  V <- t(t(alphas) %*% m$observation[,,1])  ## Note the assumption that observation is indep of action
  ## Compute optimal policy based on alpha vectors, V(b) = max_i \sum_x b(x) alpha_i(x)
  EV <- array(0, dim(Qs[[1]]))
  for(i in 1:length(Qs)){
    EV <- EV + as.matrix(Qs[[i]]) * model_prior[i]
  }

  value <- apply(EV, 1, max)
  policy <- apply(EV, 1, function(x) which.max(x))

  ## Note that policy is given as index numbers to the action vector, and state as index numbers to the states vector
  state <- 1:dim(Qs[[1]])[[1]] # For plotting convenience
  data.frame(policy, value, state)

}



## POMDP solver returns a data.frame whose columns are the alpha vectors.
## The action corresponding to each vector is given by alpha_action[i].
##
## Each alpha vector is of length n_states,  but there is not one vector for each
## action -- some actions are not represented, others may be repeated (depends on #
## of piecewise linear segments used to approximate value)
##
## So we create a new data.frame whose i'th column is the alpha vector for the i'th action
regularize_alpha <- function(alpha, alpha_action, n_a){
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






###############

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

## Belief state & parameter state evolve through time! see equations 16 & 17

bayes_update_pomdp <- function(qt, x_t, x_t1, a_t, transition, observation){

  n_models <- length(transition)
  P <- vapply(1:n_models, function(m) transition[[m]][x_t,x_t1,a_t], numeric(1))
  qt * P / sum(qt * P)

}



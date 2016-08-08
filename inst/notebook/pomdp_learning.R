## Assumes through-out that observation does not depend on action, and thus policy is determiend by observation alone and not observation + previous action

pomdp_planning <- function(models, discount, model_prior = NULL, states_prior = NULL, verbose = TRUE, mc.cores = 1L, ...){
  alphas <- init_models(models, discount, states_prior, verbose, mc.cores, ...)
  compute_policy(alphas, models, model_prior, states_prior)
}


## Simply a parallel loop over run_pomdp for each model
init_models <- function(models, discount, states_prior = NULL,
                          verbose = TRUE, mc.cores = 1L, ...){

  n_states <- dim(models[[1]]$transition)[[1]]
  n_actions <- dim(models[[1]]$transition)[[3]]
  if(is.null(states_prior)){
    states_prior <- rep(1, n_states) / n_states
  }

  parallel::mclapply(models, function(m){
    run_pomdp(m$transition, m$observation, m$reward, discount, states_prior, verbose, ...)
  }, mc.cores = mc.cores)

}

compute_policy <- function(alphas, models, model_prior = NULL, states_prior = NULL, a_0 = 1){

  n_states <- dim(alphas[[1]])[[1]]
  n_actions <- dim(alphas[[1]])[[2]]
  n_obs <- dim(models[[1]]$observation)[[2]]
  n_models <- length(models)
  if(is.null(states_prior)){
    states_prior <- rep(1, n_states) / n_states
  }
  if(is.null(model_prior)){
    model_prior <- rep(1, n_models) / n_models
  }

  ## EV[i,j] is value of obs_state[i] when taking action j
  EV <- array(0, dim = c(n_states, n_actions))
  for(j in 1:n_models){
    m <- models[[j]]

    ## belief[k,i] is belief system is in state k given observed state i
    belief <- vapply(1:n_obs, function(i){
      b <- states_prior %*% t(m$transition[,,a_0]) * m$observation[,i,a_0]
      b / sum(b)
    }, numeric(n_states))

    ## average value (b_x alpha_x) over models:  E( E( b_m(x) alpha_m(x) ) P(m)
    EV <- EV + t(belief) %*% alphas[[j]]  * model_prior[j]
  }

  ## Determine optimal action and associated value
  value <- apply(EV, 1, max)
  policy <- apply(EV, 1, function(x) which.max(x))

  data.frame(policy, value, state = 1:n_states)

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



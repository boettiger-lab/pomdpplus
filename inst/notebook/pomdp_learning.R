## Simply a parallel loop over sarsop for each model
sarsop_plus <- function(models, discount, state_prior = NULL,
                          verbose = TRUE, mc.cores = 1L, ...){

  n_states <- dim(models[[1]]$transition)[[1]]
  if(is.null(state_prior)) state_prior <- rep(1, n_states) / n_states

  parallel::mclapply(models, function(m){
    appl::sarsop(m$transition, m$observation, m$reward, discount, state_prior, verbose, ...)
  }, mc.cores = mc.cores)

}

compute_plus_policy <- function(alphas, models, model_prior = NULL, state_prior = NULL, a_0 = 1){

  n_states <- dim(alphas[[1]])[[1]]
  n_actions <- dim(alphas[[1]])[[2]]
  n_obs <- dim(models[[1]]$observation)[[2]]
  n_models <- length(models)
  if(is.null(state_prior)) state_prior <- rep(1, n_states) / n_states
  if(is.null(model_prior))  model_prior <- rep(1, n_models) / n_models

  ## EV[i,j] is value of obs_state[i] when taking action j
  EV <- array(0, dim = c(n_states, n_actions))
  for(j in 1:n_models){
    m <- models[[j]]

    ## belief[k,i] is belief system is in state k given observed state i
    belief <- vapply(1:n_obs,
                     function(i){
                       b <- state_prior %*% t(m$transition[,,a_0]) * m$observation[,i,a_0]
                       if(sum(b) == 0) numeric(n_states) ## observed state i is impossible
                       else b / sum(b)
                     },
                     numeric(n_states))

    ## average value (b_x alpha_x) over models:  E( E( b_m(x) alpha_m(x) ) P(m)
    EV <- EV + t(belief) %*% alphas[[j]]  * model_prior[j]
  }

  ## Determine optimal action and associated value
  value <- apply(EV, 1, max)
  policy <- apply(EV, 1, function(x) which.max(x))

  data.frame(policy, value, state = 1:n_states)
}


###############


sim_plus <- function(models, discount, model_prior = NULL, state_prior = NULL,
                         x0, a0 = 1, Tmax, true_transition, true_observation, true_utility = NULL,
                         alphas = NULL, verbose = TRUE, mc.cores = 1L, ...){

  ## Initialize objects
  n_states <- dim(models[[1]][["observation"]])[1]
  n_obs <- dim(models[[1]][["observation"]])[2]
  n_models <- length(models)
  value <- obs <- action <- state <- numeric(Tmax+1)
  model_posterior <- array(NA, dim = c(Tmax+1, n_models))
  state_posterior <- array(NA, dim = c(Tmax+1, n_states))

  ## Defaults if not provided
  if(is.null(true_utility)) true_utility <- models[[1]][["reward"]]
  if(is.null(state_prior))  state_prior <- rep(1, n_states) / n_states
  if(is.null(model_prior))  model_prior <- rep(1, n_models) / n_models

  ## Starting values
  state[2] <- x0
  action[1] <- a0  # only relevant if action influences observation process
  model_posterior[2,] <- model_prior
  state_posterior[2,] <- state_prior

  ## If alphas are not provided, assume we are running pomdpsol
  if(is.null(alphas)){
    if(verbose) message("alphas not provided, recomputing them from SARSOP algorithm at each time step. This can be very slow!")
    update_alphas <- TRUE
  }

  ## Forward simulation, updating belief
  for(t in 2:Tmax){
    ## In theory, alphas should always be updated based on the new belief in states, but in practice the same alpha vectors can be used
    if(update_alphas) alphas <- sarsop_plus(models, discount, state_posterior[t,], verbose, mc.cores, ...)

    ## Get the policy corresponding to each possible observation, given the current beliefs
    out <- compute_plus_policy(alphas, models, model_posterior[t,], state_posterior[t,], action[t-1])

    ## Update system using random samples from the observation & transitition distributions:
    obs[t] <- sample(1:n_obs, 1, prob = true_observation[state[t], , action[t-1]])
    action[t] <- out$policy[obs[t]]
    value[t] <- true_utility[state[t], action[t]] * discount^(t-1)
    state[t+1] <- sample(1:n_states, 1, prob = true_transition[state[t], , action[t]])
    ## Bayesian update of beliefs over models and states
    model_posterior[t+1,] <- update_model_belief(state_posterior[t,], model_posterior[t,], models, obs[t], action[t-1])
    state_posterior[t+1,] <- update_state_belief(state_posterior[t,], model_posterior[t,], models, obs[t], action[t-1])

  }

  ## assemble data frame without dummy year for starting action
  df <- data.frame(time = 0:Tmax, state, obs, action, value)[2:Tmax,]
  list(df = df, model_posterior = model_posterior[2:(Tmax+1),], state_posterior = state_posterior[2:(Tmax+1),])
}


update_model_belief <-  function(state_prior, model_prior, models, observation, action){
  belief <-
  model_prior *
    vapply(models, function(m){

      state_prior %*%
        m$transition[, , action] %*%
        m$observation[, observation, action]

    }, numeric(1))
  belief / sum(belief)
}

update_state_belief <- function(state_prior, model_prior, models, observation, action){
  belief <-
  vapply(1:length(state_prior), function(i){ sum(    ## belief for each state, p_{t+1}(x_{t+1})
    vapply(1:length(models), function(k){ ## average over models

      model_prior[k] *
        state_prior %*%
        models[[k]]$transition[, i, action] *
        models[[k]]$observation[i, observation, action]

    }, numeric(1)))
  }, numeric(1))

  belief / sum(belief)

}


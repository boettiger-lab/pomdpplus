

#' sim_plus
#'
#' @param models a list of lists, each of which gives the transition matrix, observation matrix and reward matrix for the model considered
#' @param discount discount rate
#' @param model_prior Prior belief assigned to each model. uniform by default.
#' @param state_prior Prior belief that system is in state x_i before simulation starts
#' @param x0 starting state of the system
#' @param a0 initial action (used to make the first observation, only relevant if observation depends on action)
#' @param Tmax number of time steps to simulate
#' @param true_model a list of the transition matrix, observation matrix, and reward matrix used to simulate draws.
#' @param alphas the alpha vectors for each model, as provided from \code{\link{sarsop_plus}}, which will otherwise be run each time if not provided.
#' @param ... additiional options to appl::sarsop, if alphas are not provided
#'
#' @return a list with elements
#' (1) \code{df} a data.frame with the time, state, observation, action, and value at each timestep,
#' (2) \code{model_posterior}, a data.frame of Tmax rows by n_models columns giving the evolution of the belief over models, and
#' (3) \code{state_posterior}, a data.frame of Tmax rows by n_states columns giving the evolution of the belief over states
#' @export
#'
#' @examples \dontrun{
#' source(system.file("examples/K_models.R", package="pomdpplus"))
#' alphas <- sarsop_plus(models, discount, precision = 1)
#' out <- sim_plus(models = models, discount = discount,
#'                 x0 = 5, a0 = 1, Tmax = 10,
#'                 true_model = models[[2]], alphas = alphas)
#' }
#'
sim_plus <- function(models, discount, model_prior = NULL, state_prior = NULL,
                     x0, a0 = 1, Tmax, true_model,
                     alphas = NULL, ...){

  ## Initialize objects
  n_states <- dim(models[[1]][["observation"]])[1]
  n_obs <- dim(models[[1]][["observation"]])[2]
  n_models <- length(models)
  value <- obs <- action <- state <- numeric(Tmax+1)
  model_posterior <- array(NA, dim = c(Tmax+1, n_models))
  state_posterior <- array(NA, dim = c(Tmax+1, n_states))

  ## Defaults if not provided
  if(is.null(state_prior))  state_prior <- rep(1, n_states) / n_states
  if(is.null(model_prior))  model_prior <- rep(1, n_models) / n_models

  ## Assign starting values
  state[2] <- x0
  action[1] <- a0  # only relevant if action influences observation process
  model_posterior[2,] <- model_prior
  state_posterior[2,] <- state_prior

  ## If alphas are not provided, assume we are running pomdpsol each time
  if(is.null(alphas)){
    message("alphas not provided, recomputing them from SARSOP algorithm at each time step. This can be very slow!")
    update_alphas <- TRUE
  } else {
    update_alphas <- FALSE
  }


  ## Forward simulation, updating belief
  for(t in 2:Tmax){
    ## In theory, alphas should always be updated based on the new belief in states, but in practice the same alpha vectors can be used
    if(update_alphas) alphas <- sarsop_plus(models, discount, state_posterior[t,], ...)

    ## Get the policy corresponding to each possible observation, given the current beliefs
    policy <- compute_plus_policy(alphas, models, model_posterior[t,], state_posterior[t,], action[t-1])

    ## Update system using random samples from the observation & transition distributions:
    obs[t] <- sample(1:n_obs, 1, prob = true_model$observation[state[t], , action[t-1]])
    action[t] <- policy$policy[obs[t]]
    value[t] <- true_model$reward[state[t], action[t]] * discount^(t-1)
    state[t+1] <- sample(1:n_states, 1, prob = true_model$transition[state[t], , action[t]])

    ## Bayesian update of beliefs over models and states
    model_posterior[t+1,] <- update_model_belief(state_posterior[t,], model_posterior[t,], models, obs[t], action[t-1])
    state_posterior[t+1,] <- update_state_belief(state_posterior[t,], model_posterior[t,], models, obs[t], action[t-1])

  }

  ## assemble data frame without dummy year for starting action
  df <- data.frame(time = 0:Tmax, state, obs, action, value)[2:Tmax,]
  list(df = df,
       model_posterior = as.data.frame(model_posterior[2:(Tmax+1),]),
       state_posterior = as.data.frame(state_posterior[2:(Tmax+1),]))
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


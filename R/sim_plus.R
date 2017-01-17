

#' sim_plus
#'
#' @param models a list of lists, each of which gives the transition matrix, observation matrix and reward matrix for the model considered
#' @param discount discount rate
#' @param model_prior Prior belief assigned to each model. uniform by default.
#' @param state_prior Prior belief that system is in state x_i and model m_i
#' @param x0 starting state of the system
#' @param a0 initial action (used to make the first observation, only relevant if observation depends on action)
#' @param Tmax number of time steps to simulate
#' @param true_model a list of the transition matrix, observation matrix, and reward matrix used to simulate draws.
#' @param alphas the alpha vectors for each model, as provided from \code{\link{sarsop_plus}}, which will otherwise be run each time if not provided.
#' @param model_names vector of identifying names for each model. If none are provided, model posterior columns will be named V1, V2, etc.
#' @param state_names vector of identifying names for each state.  If none are provided, state posterior columns will be named V1, V2, etc.
#' @param ... additional options to appl::sarsop, if alphas are not provided
#'
#' @return a list with elements
#' (1) \code{df} a data.frame with the time, state, observation, action, and value at each timestep,
#' (2) \code{model_posterior}, a data.frame of Tmax rows by n_models columns giving the evolution of the belief over models, and
#' (3) \code{state_posterior}, a data.frame of Tmax rows by n_states columns giving the evolution of the belief over states
#' @export
#'
#' @examples \dontrun{
#' source(system.file("examples/K_models.R", package="pomdpplus"))
#' alphas <- sarsop_plus(models, discount, precision = .1)
#' out <- sim_plus(models = models, discount = discount,
#'                 x0 = 6, a0 = 1, Tmax = 10,
#'                 true_model = models[[2]], alphas = alphas)
#' }
#'
sim_plus <- function(models, discount, prior = NULL,
                     x0, a0 = 1, Tmax, true_model, alphas = NULL,
                     model_names = NULL, ...){

  if(any(is.null(model_names)))
    model_names <- names(models)

  ## Initialize objects
  n_states <- dim(models[[1]][["observation"]])[1]
  n_obs <- dim(models[[1]][["observation"]])[2]
  n_models <- length(models)
  value <- obs <- action <- state <- numeric(Tmax+1)
  posterior <- array(NA, dim = c(n_models, n_states, Tmax+1))


  ## Defaults if not provided
  if(is.null(prior))
    prior <- outer(rep(1, n_models) / n_models, rep(1, n_states) / n_states)

  ## Assign starting values
  state[2] <- x0
  action[1] <- a0  # only relevant if action influences observation process
  posterior[,,2] <- prior



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
    if(update_alphas) alphas <- sarsop_plus(models, discount, posterior[,,t], ...)

    ## Get the policy corresponding to each possible observation, given the current beliefs
    policy <- compute_plus_policy(alphas, models, posterior[,,t], action[t-1])

    ## Update system using random samples from the observation & transition distributions:
    obs[t] <- sample(1:n_obs, 1, prob = true_model$observation[state[t], , action[t-1]])
    action[t] <- policy$policy[obs[t]]
    value[t] <- true_model$reward[state[t], action[t]] * discount^(t-1)
    state[t+1] <- sample(1:n_states, 1, prob = true_model$transition[state[t], , action[t]])

    ## Bayesian update of beliefs over models and states
    #posterior[,,t+1] <- update_plus_belief(posterior[,,t], models, obs[t], action[t-1])

    posterior_model[,t+1] <- update_plus_belief(posterior[,,t], models, obs[t], action[t-1], posterior_model[,t])
    posterior[,,t+1] <- update_plus_belief(posterior[,,t], models, obs[t], action[t-1])
  }

  ## assemble data frame without dummy year for starting action
  df <- data.frame(time = 0:Tmax, state, obs, action, value)[2:Tmax,]

  list(df = df,
       posterior = posterior[,,-1])
}

normalize <- function(x){
  s <- sum(x)
  x / (s + (s == 0))
}


update_state_belief <- function(posterior, models, z0, a0){
  posterior <- vapply(1:length(models), function(i){
     normalize( state_belief %*% models[[i]]$transition[, , a0] * models[[i]]$observation[, z0, a0] )
  }, numeric(dim(posterior)[[2]]) )
  t(posterior)
}


update_model_belief <- function(posterior, models, z0, a0, model_belief){
  vapply(1:length(models), function(i){
    state_belief <- t(normalize(posterior[i,]))
    (state_belief %*% models[[i]]$transition[, , a0]) %*% models[[i]]$observation[, z0, a0] * model_belief[i]
  }, numeric(dim(posterior)[[1]]) )
}


update_plus_belief <- function(posterior, models, z0, a0){

  model_belief <- rowSums(posterior)
  posterior <- vapply(1:length(models), function(i){
    state_belief <- t(normalize(posterior[i,]))
    b <- normalize( state_belief %*% models[[i]]$transition[, , a0] * models[[i]]$observation[, z0, a0] )
    p <- (state_belief %*% models[[i]]$transition[, , a0]) %*% models[[i]]$observation[, z0, a0] * model_belief[i]
    b * as.numeric(p)

  }, numeric(dim(posterior)[[2]]) )

  t(posterior)
}



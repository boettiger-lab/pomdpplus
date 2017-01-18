

#' compare_plus
#'
#' @inheritParams sim_plus
#' @param obs vector of historical observations. Must use index value of (discrete) observation
#' @param action corresponding vector of historical actions. Must use index value of (discrete) action
#' @return a list with elements
#' (1) \code{df} a data.frame with the time, state, observation, action, and value at each timestep,
#' (2) \code{model_posterior}, a data.frame of Tmax rows by n_models columns giving the evolution of the belief over models, and
#' (3) \code{state_posterior}, a data.frame of Tmax rows by n_states columns giving the evolution of the belief over states
#' @export
#'
#' @examples \dontrun{
#' source(system.file("examples/K_models.R", package="pomdpplus"))
#' alphas <- sarsop_plus(models, discount, precision = 1)
#' out <- compare_plus(models = models, discount = discount,
#'                 obs = rnorm(21, 15, 0.1),
#'                 action = rep(1, 21),
#'                 alphas = alphas)
#' }
#'
compare_plus <- function(models, discount, model_prior = NULL, state_prior = NULL,
                         obs, action, alphas = NULL, model_names = NULL, ...){


  if(any(is.null(model_names)))
    model_names <- names(models)

  ## Initialize objects
  Tmax <- length(obs)
  n_states <- dim(models[[1]][["observation"]])[1]
  n_models <- length(models)
  optimal <- numeric(Tmax)
  optimal[1] <- NA
  model_posterior <- array(NA, dim = c(Tmax, n_models))
  state_posterior <- array(NA, dim = c(Tmax, n_models, n_states))
  ## Defaults if not provided
  if(is.null(model_prior))  model_prior <- rep(1, n_models) / n_models
  if(is.null(state_prior))
    state_prior <- outer(rep(1, n_models), rep(1, n_states) / n_states)
  ## Assign starting values
  model_posterior[1,] <- model_prior
  state_posterior[1,,] <- state_prior

  ## Forward iteration, updating belief
  for(t in 2:Tmax){
    policy <- compute_plus_policy(alphas, models, model_posterior[t-1,], state_posterior[t-1,,], action[t-1])
    optimal[t] <- policy$policy[obs[t]]
    model_posterior[t,] <- update_model_belief(state_posterior[t-1,,], models, obs[t], action[t-1], model_posterior[t-1,])
    state_posterior[t,,] <- update_state_belief(state_posterior[t-1,,], models, obs[t], action[t-1])
  }
  ## assemble data frame without dummy year for starting action


  df = data.frame(time = 1:Tmax, obs, action, optimal)
  model_posterior = as.data.frame(model_posterior)

  if(!any(is.null(model_names)))
    names(model_posterior) <- model_names

  list(df = df,
       model_posterior = model_posterior,
       state_posterior = state_posterior)

}





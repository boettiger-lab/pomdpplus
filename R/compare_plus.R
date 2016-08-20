

#' compare_plus
#'
#' @param models a list of lists, each of which gives the transition matrix, observation matrix and reward matrix for the model considered
#' @param discount discount rate
#' @param model_prior Prior belief assigned to each model. uniform by default.
#' @param state_prior Prior belief that system is in state x_i before simulation starts
#' @param obs vector of observations from given data
#' @param action vector of actions corresponding to each observation, given the data
#' @param alphas the alpha vectors for each model, as provided from \code{\link{sarsop_plus}}, which will otherwise be run each time if not provided.
#' @param verbose should function provide messages?
#' @param mc.cores number of parallel cores. Only relevant if alphas not provided. Be sure adequate memory is available first!
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
#' out <- compare_plus(models = models, discount = discount,
#'                 obs = rnorm(21, 15, 0.1),
#'                 action = rep(1, 21),
#'                 alphas = alphas)
#' }
#'
compare_plus <- function(models, discount, model_prior = NULL, state_prior = NULL,
                     obs, action,
                     alphas = NULL, verbose = TRUE, mc.cores = 1L, ...){
  ## Initialize objects
  Tmax <- length(obs) - 1
  n_states <- dim(models[[1]][["observation"]])[1]
  n_models <- length(models)
  optimal <- numeric(Tmax+1)
  model_posterior <- array(NA, dim = c(Tmax+1, n_models))
  state_posterior <- array(NA, dim = c(Tmax+1, n_states))
  ## Defaults if not provided
  if(is.null(state_prior))  state_prior <- rep(1, n_states) / n_states
  if(is.null(model_prior))  model_prior <- rep(1, n_models) / n_models
  ## Assign starting values
  model_posterior[2,] <- model_prior
  state_posterior[2,] <- state_prior

  ## Forward iteration, updating belief
  for(t in 2:Tmax){
    policy <- compute_plus_policy(alphas, models, model_posterior[t,], state_posterior[t,], action[t-1])
    optimal[t] <- policy$policy[obs[t]]
    model_posterior[t+1,] <- update_model_belief(state_posterior[t,], model_posterior[t,], models, obs[t], action[t-1])
    state_posterior[t+1,] <- update_state_belief(state_posterior[t,], model_posterior[t,], models, obs[t], action[t-1])
  }
  ## assemble data frame without dummy year for starting action
  df <- data.frame(time = 0:Tmax, obs, action, optimal)[2:Tmax,]
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


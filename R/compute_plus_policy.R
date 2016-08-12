
#' compute_plus_policy
#'
#' @inheritParams sim_plus
#'
#' @return a data frame containing the policy and value associated with each possible observation
#' @export
#'
#' @examples
#' \dontrun{
#' source(system.file("examples/K_models.R", package="pomdpplus"))
#' alphas <- sarsop_plus(models, discount, precision = 1)
#' unif <- compute_plus_policy(alphas, models)
#' }
compute_plus_policy <- function(alphas, models, model_prior = NULL, state_prior = NULL, a0 = 1){

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
                       b <- state_prior %*% t(m$transition[, , a0]) * m$observation[, i, a0]
                       if(sum(b) == 0) numeric(n_states) ## observed state i is impossible
                       else b / sum(b)
                     },
                     numeric(n_states))
    EV <- EV + t(belief) %*% alphas[[j]]  * model_prior[j]
  }

  value <- apply(EV, 1, max)
  policy <- apply(EV, 1, function(x) which.max(x))
  data.frame(policy, value, state = 1:n_obs)
}

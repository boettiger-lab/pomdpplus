
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
    
    n_states <- dim(models[[1]]$observation)[[1]]
    n_actions <- dim(models[[1]]$observation)[[3]]
    n_obs <- dim(models[[1]]$observation)[[2]]
    n_models <- length(models)
    n_alpha <- length(alphas)
    if(is.null(state_prior)){
        state_prior = array(0, dim = c(length(models),n_states))
        for(i in 1:length(models))
        state_prior[i,] <- rep(1, n_states) / n_states
    }
    if(is.null(model_prior))  model_prior <- rep(1, n_models) / n_models
    
    
    EV <- array(0, c(n_states, n_actions))
    for(j in 1:n_models){
        m <- models[[j]]
        ## belief[k,i] is belief system is in state k given observed state i
        belief <- vapply(1:n_obs,
        function(i){
            b <- state_prior[j,] %*% m$transition[, , a0] * m$observation[, i, a0]
            if(sum(b) == 0) numeric(n_states) ## observed state i is impossible
            else b / sum(b)
        },
        numeric(n_states))
        V <- t(belief) %*% alphas[[j]]$vectors  * model_prior[j]
        EV <- EV + regularize_V(V, alphas[[j]]$action, n_actions)
    }
    
    value <- apply(EV, 1, max)
    policy <- apply(EV, 1, function(x) which.max(x))
    
    data.frame(policy, value, state = 1:n_obs)
}


regularize_V <- function(V, alpha_action, n_a){
    n_x <- dim(V)[[1]]
    vapply(1:n_a, function(i){
        j <- which(alpha_action == i)
        if(length(j) > 0){
            if(length(j) > 1){
                apply(V[, j], 1, max)
            } else {
                V[,j]
            }
        } else {
            rep(0, n_x)
        }
    }, numeric(n_x))
}


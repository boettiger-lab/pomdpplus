#' sarsop_plus
#'
#' @inheritParams sim_plus
#' @param mc.cores number of parallel cores to use
#' @param log_data a data frame with information to be logged, one row for each model. Leave NULL if not logging
#' @importFrom appl sarsop
#' @return A list of alpha vector matrices
#' @export
#'
#' @examples \dontrun{
#' #' source(system.file("examples/K_models.R", package="pomdpplus"))
#' alphas <- sarsop_plus(models, discount, precision = 1)
#' unif <- compute_plus_policy(alphas, models, c(0.5, 0.5))
#' }
sarsop_plus <- function(models, discount, state_prior = NULL,
mc.cores = 1L, log_data = NULL, ...){
    
    n_states <- dim(models[[1]]$transition)[[1]]
    if(is.null(state_prior)){
        state_prior = array(0, dim = c(length(models),n_states))
        for(i in 1:length(models))
        state_prior[i,] <- rep(1, n_states) / n_states
    }
    
    parallel::mclapply(1:length(models), function(i){
        
        m <- models[[i]]
        if(!is.null(log_data) && dim(log_data)[1] > 1)
        log_dat <- log_data[i,]
        else
        log_dat <- log_data
        
        appl::sarsop(m$transition, m$observation, m$reward, discount, state_prior[i,], log_data = log_dat, ...)
    }, mc.cores = mc.cores)
    
}

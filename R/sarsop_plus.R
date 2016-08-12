#' sarsop_plus
#'
#' @inheritParams sim_plus
#'
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
                        verbose = TRUE, mc.cores = 1L, ...){

  n_states <- dim(models[[1]]$transition)[[1]]
  if(is.null(state_prior)) state_prior <- rep(1, n_states) / n_states

  parallel::mclapply(models, function(m){
    appl::sarsop(m$transition, m$observation, m$reward, discount, state_prior, verbose, ...)
  }, mc.cores = mc.cores)

}

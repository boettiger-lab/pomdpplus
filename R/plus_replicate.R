#' plus_replicate
#'
#' Replicate sim_plus calls, potentially in parallel. Can also replicate mdp_learning
#' and mdp_planning simulation calls
#' @param reps number of replicates to simulate
#' @param expr expression to replicate; must return a
#' @param ... additional arguments to \code{\link{mclapply}}, such as mc.cores
#' @return Same structure as returned by the given expression (e.g. sim_plus),
#'  but with replicates appended as additional rows, with an indicator column
#'  @importFrom parallel mclapply
#'  @export
#'
plus_replicate <- function(reps, expr, ...){

  out <- parallel::mclapply(integer(reps),
                            eval.parent(substitute(function(...) expr)), ...)

  if(is.data.frame(out[[1]])){
    return(bind_rows(out, .id = "rep"))

  } else {
    df <- bind_rows(lapply(out, `[[`, 1), .id = "rep")

    state_posterior <- model_posterior <- NULL

    if(length(out[[1]]) > 1)
      model_posterior <- bind_rows(lapply(out, `[[`, 2), .id = "rep")

    if(length(out[[1]]) == 3)
      state_posterior <- bind_rows(lapply(out, `[[`, 3), .id = "rep")

    list(df=df,
         model_posterior = model_posterior,
         state_posterior = state_posterior)
  }
}

## Avoid introducing a hard dplyr dependency
bind_rows <- function(..., .id = NULL){
  L <- list(...)
  out <- do.call(rbind, lapply(1:length(L), function(i)
    data.frame(rep = i, L[[i]])))
  names(out)[1] <- .id
  out
}

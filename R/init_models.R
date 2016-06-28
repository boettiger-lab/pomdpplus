#' function for generating approimation of value function for each candidate model POMDP
#'
#' @param T list of transition probabilities matrices for all candidate models; length = Num_model
#' @param O list of emission probabilities matrices for all candidate models; length = Num_model
#' @param R matrix of reward function
#' @param GAMMA discount factor
#' @param Num_model number of candidate models
#' @param initial initial belief over the states
#' @return av list of alpha vectors for all candidate models; length = Num_Model
#' @return aa list of actions corresponding to alpha vectors for all candidate models; length = Num_Model
#' @importFrom appl write_pomdpx pomdpsol read_policy
#' @export
init_models <- function(T,O,R,GAMMA,Num_model = length(T),initial = NULL) {

  Num_s = dim(T[[1]])[1]
  Num_a = dim(T[[1]])[3]

  if(is.null(initial))
    initial <- array(1, dim = Num_s) / Num_s


  av = vector('list',Num_model)
  aa = vector('list',Num_model)


  for(i in 1:Num_model){

    appl::write_pomdpx(T[[i]],O[[i]],R,GAMMA,initial)
    appl::pomdpsol("input.pomdpx", "pomdp.policy", precision = 0.001, timeout = 1000)
    out = appl::read_policy(initial, file = "pomdp.policy")

    av[[i]] = out[[3]]
    aa[[i]] = out[[4]] + 1
  }

  output = list(av = av, aa = aa)

}

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
#' @importFrom xml2 read_xml xml_find_all xml_contents xml_attr
#' @importFrom purrr map_dbl
#' @importFrom appl writepomdp pomdpsol read_policy
init_models <- function(T,O,R,GAMMA,Num_model = length(T),initial) {

Num_s = dim(T[[1]])[1]
Num_a = dim(T[[1]])[3]
av = vector('list',Num_model)
aa = vector('list',Num_model)


for(i in 1:Num_model){

  appl::writepomdpx_POMDP(T[[i]],O[[i]],R,GAMMA,initial)
  appl::pomdpsol("input.pomdpx", "pomdp.policy", precision = 0.001, timeout = 1000)
  out = appl::read_policy(initial, file = "pomdp.policy")

  av[[i]] = out[[3]]
  aa[[i]] = out[[4]] + 1
}

output = list(av,aa)

}

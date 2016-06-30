#' main code for planning and learning in uncertain systems
#'
#' @param input A list of transition (itself a list for all candidate models),
#' emission (itslef a list for all candidate models), reward matrix, and discount factor.
#' @param Num_sim Number of simulations replicates ; default = 100
#' @param seq list containing the sequnce of actions and observations from environment
#' @param n_sample number of sample to use for planning under model uncertainty; default = 5
#' @param initial initial belief state
#' @param P prior probability of the models ; default is flat prior
#' @param precision accuracy of value function for generating models
#' @return df data farme including results of forward simulations
#' @return posterior data frame of Posterior distribution of each candidate model at each time
#' @return av list of alpha vectors for all candidate models; length = Num_Model
#' @return aa list of actions corresponding to alpha vectors for all candidate models; length = Num_Model
#' @export
plus_seq <- function(input, Num_sim = 100, seq, n_sample = 5, initial, P = (array(1,dim = length(input[[1]]))/ length(input[[1]])),precision = 0.001){

  # extracting model parameters from input
  T = input[[1]]
  O = input[[2]]
  R = input[[3]]
  GAMMA = input[[4]]
  
  # initialize the problem
  Num_s = dim(O[[1]])[1]   # dimension of states
  Num_z = dim(O[[1]])[2]   # dimension of observations
  Num_a = dim(O[[1]])[3]   # dimension of actions
  N = length(T)            # Number of candidate models
  
  ######################################################################################
  # initializing models for each candidate
  out1 <- init_models(T,O,R,GAMMA,N,initial,precision)
  
  av = out1[[1]]
  aa = out1[[2]]
  
  ######################################################################################
  # running the simulation

  out2 <- run_sim_seq(T,O,R,GAMMA,av,aa,Num_sim,seq,N,initial,n_sample,P)
  
  
  df = out2[[1]]
  posterior = out2[[2]]
  
  
  list(df = df ,posterior = posterior ,av = av ,aa = aa)
  
  
  
  
}

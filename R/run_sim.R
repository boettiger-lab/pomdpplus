### INPUTS

# T: list of transition functions for all candidate models
# O: list of emission functions for all candidate models
# R: matrix of reward function
# GAMMA: discount factor
# av: list of alpha vectors for all candidate models
# aa: list of actions corresponding to alpha vectors for all candidate models
# n: index of the true model
# Num_sim: number of simulation replicates
# t: time horizon of the simulaitons
# N: number of candidate models
# init: initial belief
# n_sample: number of samples used for planning
# P: prior distribution over models

run_sim <- function(T,O,R,GAMMA,av,aa,n,Num_sim,t,N,init,n_sample, P = (ones(1,length(T)) / length(T))){
  
  # initial state
  s_0 = discrete(init,1)
  
  # true model
  T_star = T[[n]]; O_star = O[[n]]
  aa_star = aa[[n]]
  av_star = av[[n]]
  
  # dimensions of the problem
  Num_s = dim(T_star)[1]
  Num_z = dim(O_star)[2]
  Num_a = dim(T_star)[3]
  # sequence of states
  state_seq_mdp_star = zeros(Num_sim,t+1)
  state_seq_mdp_pl = zeros(Num_sim,t+1)
  
  # true model sequence of act, obs, rew, and val
  history_star_act = zeros(Num_sim,t+1)
  history_star_rew = zeros(Num_sim,t+1)
  history_star_obs = zeros(Num_sim,t+1)
  # PLUS model sequence of act, obs, rew, and val
  history_pl_act = zeros(Num_sim,t+1)
  history_pl_rew = zeros(Num_sim,t+1)
  history_pl_obs = zeros(Num_sim,t+1)
  # belief of true model
  b_star = zeros(Num_sim,t+1,Num_s)
  # belief of the plus method
  b_pl = vector('list',N)
  for(i in 1:N){
    b_pl[[i]] = array(0,dim = c(Num_sim,t+1,Num_s))
  }
  
  # track of posterior probability of candidate models over time
  PP_pl  = zeros(Num_sim,t+1,N)           

  # let's start the simulations
  for(m in 1:Num_sim){
    
    # initialize
    state_seq_mdp_star[m,1] = s_0
    state_seq_mdp_pl[m,1] = s_0
    b_star[m,1,]= init
    for(i in 1:N){
      b_pl[[i]][m,1,] = init
    }
    
    # sample models for planning
    ind_sam = discrete(P,n_sample)
    aa_sam = vector('list',n_sample)
    av_sam = vector('list',n_sample)
    T_sam = vector('list',n_sample)
    belief = zeros(n_sample,Num_s)
    for(i in 1:n_sample){
      T_sam[[i]] = T[[ind_sam[i]]]
      aa_sam[[i]] = aa[[ind_sam[i]]]
      av_sam[[i]] = av[[ind_sam[i]]]
      belief[i,] = b_pl[[ind_sam[i]]][m,1,]
      w[i] = P[ind_sam[i]]
    }
    
    # taking the first action for true model
    out = Interp_MM(b_star[m,1,],av_star,aa_star)
    history_star_act[m,1] = out[[2]]
    
    # taking the first action for plus
    out <- Expectation_pl(belief,av_sam,aa_sam,Num_a,w)
    history_pl_act[m,1] = out[[2]]
    
    # initializing the priors over candidate models
    PP_pl[m,1,] = P
    
    # starting the forward run
    for(tt in 1:t){
      # getting the observation from the environment
      # true model
      out_star <- fakemodel(T_star,O_star,state_seq_mdp_star[m,tt],history_star_act[m,tt],R,Num_s,Num_z)
      state_seq_mdp_star[m,tt+1] = out_star[[1]]
      history_star_obs[m,tt] = out_star[[2]]
      history_star_rew[m,tt] = out_star[[3]]
      # updating the belief
      b_star[m,tt+1,] = update_belief(b_star[m,tt,],T_star,O_star,history_star_act[m,tt],history_star_obs[m,tt]) 
      
      # PLUS
      out_pl <- fakemodel(T_star,O_star,state_seq_mdp_pl[m,tt],history_pl_act[m,tt],R,Num_s,Num_z)
      state_seq_mdp_pl[m,tt+1] = out_pl[[1]]
      history_pl_obs[m,tt] = out_pl[[2]]
      history_pl_rew[m,tt] = out_pl[[3]]
      
      # Learning phase for plus
      
      for(i in 1:length(P)){
        act = history_pl_act[m,tt]
        obs = history_pl_obs[m,tt]
        # forward filtering
        out_pl <- ff(b_pl[[i]][m,tt,],T[[i]],O[[i]],act,obs,PP_pl[m,tt,i])
        PP_pl[m,tt+1,i] = out_pl[[1]]
        b_pl[[i]][m,tt+1,] = out_pl[[2]]
      }
      PP_pl[m,tt+1,] = normalize(PP_pl[m,tt+1,])
      
      # taking next action
      
      # true model
      out = Interp_MM(b_star[m,tt+1,],av_star,aa_star)
      history_star_act[m,tt+1] = out[[2]]
      
      
      # PLUS
      ind_sam = discrete(PP_pl[m,tt+1,],n_sample)
      w = array(0,dim = c(n_sample))
      for(i in 1:n_sample){
        T_sam[[i]] = T[[ind_sam[i]]]
        aa_sam[[i]] = aa[[ind_sam[i]]]
        av_sam[[i]] = av[[ind_sam[i]]]
        belief[i,] = b_pl[[ind_sam[i]]][m,tt+1,]
        w[i] = PP_pl[m,tt+1,ind_sam[i]]
      }
      w = normalize(w)
      out <- Expectation_pl(belief,av_sam,aa_sam,Num_a,w)
      history_pl_act[m,tt+1] = out[[2]]
      
    }
    
    
  }
 
  output = list(history_star_rew,history_star_act,history_pl_rew,
                history_pl_act,state_seq_mdp_pl,state_seq_mdp_star,PP_pl)
  
}

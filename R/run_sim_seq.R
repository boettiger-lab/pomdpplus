#' function for running forward simulations for planning and learning under model uncertainty
#'
#' @param T list of transition probabilities matrices for all candidate models; length = Num_model
#' @param O list of emission probabilities matrices for all candidate models; length = Num_model
#' @param R matrix of reward function
#' @param GAMMA discount factor
#' @param av list of alpha vectors for all candidate models; length = Num_Model
#' @param aa list of actions corresponding to alpha vectors for all candidate models; length = Num_Model
#' @param Num_sim number of simulation replicates; default = 100
#' @param seq list containing the sequnce of actions and observations from environment
#' @param N number of candidate models
#' @param init initial belief of the states
#' @param n_sample number of samples used for planning; default = 5
#' @param P prior probability of the models ; default is flat prior
#' @return df data farme including results of forward simulations
#' @return posterior data frame of Posterior distribution of each candidate model at each time
#' @importFrom appl read_policy write_pomdpx
#' @export
run_sim_seq <- function(T, O, R, GAMMA, av, aa, Num_sim, seq, N = length(T), init, n_sample, P = (array(1, dim = c(1,length(T))) / length(T))){
  
  obs_seq = seq[[1]]
  act_seq = seq[[2]]
  t = length(obs_seq)
  # initial state
  s_0 = discrete(init,1)
  
  # dimensions of the problem
  Num_s = dim(T[[1]])[1]
  Num_z = dim(O[[1]])[2]
  Num_a = dim(T[[1]])[3]

  # true model sequence of act, obs, rew, and val
  history_star_act = array(0, dim = c(Num_sim,t+1))
  history_star_rew = array(0, dim = c(Num_sim,t+1))
  history_star_obs = array(0, dim = c(Num_sim,t+1))
  # PLUS model sequence of act, obs, rew, and val
  history_pl_act = array(0, dim = c(Num_sim,t+1))
  history_pl_rew = array(0, dim = c(Num_sim,t+1))
  history_pl_obs = array(0, dim = c(Num_sim,t+1))
  # belief of true model
  b_star = array(0, dim = c(Num_sim,t+1,Num_s))
  # belief of the plus method
  b_pl = vector('list',N)
  for(i in 1:N){
    b_pl[[i]] = array(0,dim = c(Num_sim,t+1,Num_s))
  }
  
  # track of posterior probability of candidate models over time
  PP_pl  = array(0, dim = c(Num_sim,t+1,N))
  
  # let's start the simulations
  for(m in 1:Num_sim){
    
    # initialize
    b_star[m,1,]= init
    for(i in 1:N){
      b_pl[[i]][m,1,] = init
    }
    
    # sample models for planning
    ind_sam = discrete(P,n_sample)
    aa_sam = vector('list',n_sample)
    av_sam = vector('list',n_sample)
    T_sam = vector('list',n_sample)
    belief = array(0, dim = c(n_sample,Num_s))
    w = array(0, dim = n_sample)
    for(i in 1:n_sample){
      T_sam[[i]] = T[[ind_sam[i]]]
      aa_sam[[i]] = aa[[ind_sam[i]]]
      av_sam[[i]] = av[[ind_sam[i]]]
      belief[i,] = b_pl[[ind_sam[i]]][m,1,]
      w[i] = P[ind_sam[i]]
    }
    
    # taking the first action for true model
    history_star_act[m,1] = act_seq[1]
    
    # taking the first action for plus
    out <- Expectation_pl(belief,av_sam,aa_sam,Num_a,w)
    history_pl_act[m,1] = out[[2]]
    
    # initializing the priors over candidate models
    PP_pl[m,1,] = P
    
    # starting the forward run
    for(tt in 1:t){
      # getting the observation from the environment
      history_star_obs[m,tt] = obs_seq[tt]
      history_star_rew[m,tt] = R[obs_seq[tt],act_seq[tt]]
      # updating the belief
      # PLUS
      history_pl_obs[m,tt] = obs_seq[tt]
      history_pl_rew[m,tt] = R[obs_seq[tt],history_pl_act[m,tt]]
      
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
      if(sum(PP_pl[m,tt+1,]==0)){
        stop("None of candidate models represent the provided time series")
      }

      # taking next action
      
      # true model
      history_star_act[m,tt+1] = act_seq[tt+1]
      
      
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
  
  
  df <- expand.grid(1:(t+1),1:Num_sim)
  names(df)[1] = "time"
  names(df)[2] = "sim"
  
  col_star_act = array(0,dim = length(df[[1]]))
  col_star_rew = array(0,dim = length(df[[1]]))
  col_pl_act = array(0,dim = length(df[[1]]))
  col_pl_rew = array(0,dim = length(df[[1]]))
  k = 1
  for(i in seq(1,length(df[[1]]),by = (t+1))){
    col_star_act[i:(i+t)] =  history_star_act[k,]
    col_star_rew[i:(i+t)] =  history_star_rew[k,]
    col_pl_act[i:(i+t)] =  history_pl_act[k,]
    col_pl_rew[i:(i+t)] =  history_pl_rew[k,]
    k = k+1
  }
  
  df <- data.frame(df,(col_star_act), (col_star_rew), 
                  (col_pl_act), (col_pl_rew))
  
  names(df)[3] = "true_action"
  names(df)[4] = "true_reward"
  names(df)[5] = "plus_action"
  names(df)[6] = "plus_reward"

  
  posterior <- expand.grid(1:(t+1),1:Num_sim)
  names(posterior)[1] = "time"
  names(posterior)[2] = "sim"
  
  col_post = array(0,dim = c(length(posterior[[1]]),N))

  for(i in 1:length(posterior[[1]])){
    col_post[i,] =  PP_pl[posterior[[2]][i],posterior[[1]][i],]
  }
  
  posterior <- data.frame(posterior,col_post)
  

  for(i in 3:(3+N-1)){
    names(posterior)[i] = paste0("model_",(i-2))
  }
  
  list(df = df, posterior = posterior)
  
}

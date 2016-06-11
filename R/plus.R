### INPUTS

# input: User will provide a list of transition, emission and reward, and discount parameters
## each of the transitions is itself a list of the transitions for each candidate model
### same for emission and reward.

# t: time horizon of the problem ; default = 100
# Num_sim: Number of simulations replicates ; default = 100
# n_true: index of the true model
# n_sample: number of sample to use for planning under model uncertainty; default = 5
# initial: initial belief state
# P: prior probability of the models ; default is flat prior



plus <- function(input,t = 100, Num_sim = 100, n_true, n_sample = 5, initial, P = (array(1,dim = length(input[[1]]))/ length(input[[1]]))){
                                                                                  

devtools::load_all()
source("L_fun.R")
source("init_models.R")
source("writepomdpx_POMDP.R")
source("run_sim.R")
library("purrr")
library("stats")
library("xml2")
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
out1 <- init_models(T,O,R,GAMMA,N,initial)

av = out1[[1]]
aa = out1[[2]]

######################################################################################
# running the simulation

out2 <- run_sim(T,O,R,GAMMA,av,aa,n_true,Num_sim,t,N,initial,n_sample,P)


history_star_rew = out2[[1]]
history_star_act = out2[[2]]
history_pl_rew = out2[[3]]
history_pl_act = out2[[4]]
state_seq_mdp_pl = out2[[5]]
state_seq_mdp_star = out2[[6]]
PP_pl = out2[[7]]


output = list(history_star_rew,history_star_act,history_pl_rew,
              history_pl_act,state_seq_mdp_pl,state_seq_mdp_star,PP_pl,av,aa)

}
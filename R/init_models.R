init_models <- function(T,O,R,GAMMA,Num_model,init){
  Num_s = dim(T[[1]])[1]
  Num_a = dim(T[[1]])[3]
  av = vector('list',Num_model)
  aa = vector('list',Num_model)
  XX = paste0("a",1:Num_a)
  SS = paste0("s",1:Num_s)

  for(i in 1:Num_model){
    writepomdpx_POMDP(T[[i]],O[[i]],R,GAMMA,init)
    appl::pomdpsol("input.pomdpx", "pomdp.policy", precision = 0.001, timeout = 1000)
    out = readpolicy(init,file = "pomdp.policy")
    av[[i]] = out[[3]]
    aa[[i]] = out[[4]] + 1
  }

  output = list(av,aa)
}

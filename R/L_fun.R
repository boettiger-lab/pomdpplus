normalize <- function(A){
  
  z = sum(A)
  s = z + (z==0)
  A / s
  
}
##################################################

size <- function(x, d=NULL){
    if(is.array(x)){
        if(is.null(d))
        return(dim(x))
        else
        return(dim(x)[d])
    } else
    return(length(x))
}

##################################################
round_Milad <- function(P){
  
  if(!is.array(P)){
    P <- array(P, dim=c(length(P), 1,1))
  }
  
  num_dig = 4;
  
  for(j in 1:dim(P)[1]){
    for(l in 1:dim(P)[2]){
      P[j,l] = round(P[j,l] * (10 ^ num_dig)) / 10 ^ num_dig
    }
  }
  
  
  for(k in 1:dim(P)[1]){
    if( sum(P[k,]) != 1){
      ind = which.max(P[k,]);
      P[k,ind]=P[k,ind] + (1.0000 - sum(P[k,]))
    }
  }
  
  P
  
}

##################################################

Expectation_pl <- function(initial,alpha_vector,alpha_action,Num_a,w){
  V = vector('list',size(alpha_vector))
  for(i in 1:size(V)){
    for(j in 1:Num_a){
      V[[i]][j] = -Inf
    }
  }
  
  for(i in 1:size(alpha_vector)){
    for(j in 1:size(alpha_vector[[i]])){
      for(k in 1:Num_a){
        if(alpha_action[[i]][j] == k){
          vv = sum(initial[i,]*alpha_vector[[i]][[j]])
          V[[i]][k] = max(vv,V[[i]][k])
        }
      }
    }
  }
  V_action = array(0, dim = Num_a)
  for(i in 1:Num_a){
    summ = 0
    for(j in 1:size(V)){
      if(V[[j]][i] != -Inf){
        summ = summ + V[[j]][i] * w[j]
      }
    }
    V_action[i]  = summ
  }
  for(i in 1:length(V_action)){
    if(is.nan(V_action[i])){
      V_action[i] = -1
    }
  }
  cc = max(V_action)
  ind = which.max(V_action)
  ExpectedV  = cc
  ExpecctedPol = ind
  list(ExpectedV, ExpecctedPol)
}

##################################################

fakemodel <- function(T,O,s_0,act,R,Num_s,Num_z){
  t = length(act)
  # transition at each time step
  v_s = array(0, dim = Num_s)
  # emission at each time step
  v_z = array(0, dim = Num_z)
  
  # outputs
  # states
  v_state = array(0, dim = t)
  # observation
  v_obs = array(0, dim = t)
  # reward 
  v_rew = array(0, dim = t)
  
  s_t1 = s_0
  
  for(k in 1:t){
    a = act[k]
    v_s = T[s_t1, , a]
    
    # new state
    s_t2 = discrete(v_s,1)
    
    v_z = O[s_t2, , a]
    
    # new observation
    z_t2 = discrete(v_z,1)
    
    # recording
    v_state[k] = s_t2
    v_obs[k] = z_t2
    v_rew[k] = R[s_0,a]
    
    s_t1 = s_t2
  }
  list(v_state,v_obs,v_rew)
}

##################################################


update_belief <- function(b_0,T,O,a,z){
  b_1 = b_0 %*% T[,,a] * t(O[,z,a])
  b_1 = normalize(b_1)
  b_1
}
##################################################

ff <- function(initial,T,O,a,z,P_0){
  b = array(0, dim = c((length(a)+1),size(T,1)))
  b[1,] = initial
  for(i in 1:length(a)){
    b[i+1,] = (b[i,] %*% T[,,a[i]]*t(O[,z[i],a[i]]))
    b[i+1,] = normalize(b[i+1,])
  }
  P = (b[length(a),] %*% T[,,a[length(a)]]) %*% O[,z[length(z)],a[length(a)]] %*% P_0
  b1 = b[length(a)+1,]
  
  list(P,b1)
}



##################################################

#' @importFrom(stats, runif)
# FIXME I think this is really just the `sample` function in R
discrete <- function(p,n){
  s = array(0, dim = n)
  for (i in 1:n){
    s[i] = sum(cumsum(p) < stats::runif(1))+1
  }
  s
}

##################################################

Interp_MM = function(initial, alpha, alpha_action){
    ## Compute dot product with initial
    a <- vapply(alpha, function(x) initial %*% matrix(x, ncol=1), double(1))
    
    ## Return policy of the vector which has the biggest inner product
    #alpha_action[which.max(a)]
    
    output = list(max(a),alpha_action[which.max(a)])
}


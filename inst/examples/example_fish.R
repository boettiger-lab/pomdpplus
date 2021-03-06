states <- 0:10
actions <- states
reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95

sigma_g <- sqrt(log(1 + 0.5 / 6))
sigma_m <- sigma_g
f <- function(x, h, r = 1, K = 20){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}

kk = c(4,8); rr = c(0.5,1)
Num_model = length(kk) * length(rr)
T = vector("list", length = Num_model)
O = vector("list", length = Num_model)
n_s <- length(states)
n_a <- length(actions)
observed_states <- states
n_z <- length(observed_states)

observation <- array(0, dim = c(n_s, n_z, n_a))
for (k in 1:n_a) {
  if(sigma_m <= 0){
    observation[, , k] <- diag(n_s)
  } else {
    for (i in 1:n_s) {
      if(states[i] <= 0){ ## cannot do dlnorm with mu = log(0) = -Inf.  Cannot solve if belief has already converged
        x <- dlnorm(observed_states, -1, sigma_m)
        observation[i, , k] <- x / sum(x)
      } else {
        x <- dlnorm(observed_states, log(states[i]), sdlog = sigma_m)    # transition probability densities
        ## Normalize using CDF
        N <- plnorm(observed_states[n_s], log(states[i]), sigma_m)       # CDF accounts for prob density beyond boundary
        x <- x * N / sum(x)                                   # normalize densities to  = cdf(boundary)
        x[n_s] <- 1 - N + x[n_s]                              # pile remaining probability on boundary
        observation[i, , k] <- x                             # store as row of transition matrix
      }
    }
  }
}

kkk = 1
for(l in 1:length(kk)){
  for(j in 1:length(rr)){
    transition <- array(0, dim = c(n_s, n_s, n_a))
    for (k in 1:n_s) {
      for (i in 1:n_a) {
        nextpop <- f(states[k], actions[i],rr[j],kk[l])
        if(nextpop <= 0)
          transition[k, , i] <- c(1, rep(0, n_s - 1))
        else if(sigma_g > 0){
          x <- dlnorm(states, log(nextpop), sdlog = sigma_g)    # transition probability densities
          N <- plnorm(states[n_s], log(nextpop), sigma_g)       # CDF accounts for prob density beyond boundary
          x <- x * N / sum(x)                                   # normalize densities to  = cdf(boundary)
          x[n_s] <- 1 - N + x[n_s]                              # pile remaining probability on boundary
          transition[k, , i] <- x                             # store as row of transition matrix
        } else {
          stop("sigma_g not > 0")
        }
      }
    }
    T[[kkk]] = transition; O[[kkk]] = observation
    kkk = kkk+1
  }
}


reward <- array(0, dim = c(n_s, n_a))
for (k in 1:n_s) {
  for (i in 1:n_a) {
    reward[k, i] <- reward_fn(states[k], actions[i])
  }
}

t = 10
Num_sim = 10
n_true = 3 
n_sample = 2 
initial = array(1, dim = n_s) / n_s
P = (array(1,dim = length(T))/ length(T))




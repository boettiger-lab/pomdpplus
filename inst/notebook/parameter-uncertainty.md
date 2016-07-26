# parameter-uncertainty
Carl Boettiger  
7/25/2016  




```r
library("pomdpplus")
library("ggplot2")
library("tidyr")
library("dplyr")
```

Initialize a simple POMDP model for fisheries:


```r
set.seed(1234)
states <- 0:50
actions <- states
observed_states <- states
reward_fn <-  function(x, a) pmin(x, a)
sigma_g <- 0.28
sigma_m <- 0.28
discount <- 0.95

## Create a list of functions, each function providing the model with different parameter combinations
pars <- expand.grid(K = c(45), r = seq(0, 2, length.out = 11))
models <- apply(pars, 1, function(row) 
  function(x, h){
     s <- pmax(x - h, 0)
     s * exp(row[["r"]] * (1 - s / row[["K"]]) )
  })
    

matrices <- lapply(models, function(model) 
  appl::fisheries_matrices(states = states, actions = actions, 
                           observed_states = states, reward_fn = reward_fn,
                           f = model, sigma_g = sigma_g, sigma_m = sigma_m))


transition <- lapply(matrices, `[[`, "transition")
utility <- matrices[[5]]$reward
Tmax <- 20

## initial prior over models
qt <- rep(1, length(models)) / length(models)
#qt <- numeric(length(models))
#qt[5] <- 1
```


transition is an array of probabilities $P(x_{t+1} | x_t, a_t)$ where `transition[i,j,k]` 
refers to probability for  `x_t = state[i]`, `x_{t+1} = state[j]`, and `a_t = action[k]`.

First we consider the optimal solution where we simply average over the uncertainty



```r
value_iteration <- function(transition, utility, qt, Tmax){
  
  n_models <- length(transition)
  n_states <- dim(transition[[1]])[1]
  n_actions <- dim(transition[[1]])[3]
  
  ## Initialize value-to-go (integrated over models)
  Vtplus <- numeric(n_states)
  ## Initialize policy & final value
  policy <- numeric(n_states)
  ## Intialize value-to-go over states i for model j
  V_model <- array(dim=c(n_states, n_models))
  
  for (t in (Tmax - 1):1) {
  # We define a matrix Q that stores the updated action values for 
  # all states (rows), actions (columns)
    Q <- array(0, dim = c(n_states, n_actions))
    for (i in 1:n_actions) {
    # For each action we fill for all states values (rows) 
    # the value associated with that action (ith column) into matrix Q
    # The utility of the ith action recorded for each state is 
    # added to the discounted average (product) of the transition matrix of the ith 
    # action by the running action value Vt 
      
      ## In the case of parametric/model uncertainty, we compute this value for each model..
      for(j in 1:n_models){
         V_model[,j] <- transition[[j]][,,i] %*% Vtplus
      }
      
      ## and then average over models weighted by qt
      Q[,i] <- utility[, i] + discount * V_model %*% qt
     
    } # end of the actions loop
  
    # Find the optimal action value at time t is the maximum of Q
    Vt <- apply(Q, 1, max)
  
    # After filling vector Vt of the action values at all states, we 
    # update the vector Vt+1 to Vt and we go to the next step standing 
    # for previous time t-1, since we iterate backward
    Vtplus <- Vt
  
  } # end of the time loop
  
  # Find optimal action for each state
  for (k in 1:n_states) {
  # We look for each state which column of Q corresponds to the 
  # maximum of the most recent value, Vt (at time t + 1). 
  # If the index vector is longer than 1, (i.e. if there is more
  # than one equally optimal value) we chose the smaller action (min harvest)
      policy[k] <- min(which(Q[k, ] == Vt[k]))
  }

  list(policy = policy, value = Vt)  
}  
```



```r
out <- value_iteration(transition, utility, qt, Tmax)
plot(states, states - actions[out$policy], xlab="Population size", ylab="Escapement")
```

![](parameter-uncertainty_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



Compare policies arising from different (prior) beliefs (and different models). What is the general impact of parameter uncertainty on the policy?  

In the Ricker model, a larger $r$ means a lower optimal escapement. Compare a uniform prior over $r \in [0,2]$ to certainty that r is either very small (0.4) or very large (1.8): 


```r
qt <- rep(1, length=length(transition)) / length(transition)
unif <- value_iteration(transition, utility, qt, Tmax)

qt <- numeric(length(transition))
qt[10] <- 1
certain_high <- value_iteration(transition, utility, qt, Tmax)

qt <- numeric(length(transition))
qt[6] <- 1
certain_mean <- value_iteration(transition, utility, qt, Tmax)


qt <- numeric(length(transition))
qt[3] <- 1
certain_low <- value_iteration(transition, utility, qt, Tmax)
```

Not surprisingly, we see that the optimal policy is intermediate (though more pessimistic than had we assumed prior mean):


```r
data.frame(states = states, 
           unif = unif$policy, 
           certain_low = certain_low$policy, 
           certain_high = certain_high$policy,
           certain_mean = certain_mean$policy) %>% 
  gather(prior, policy, -states) %>%
  ggplot(aes(states, states - actions[policy], col=prior, lty=prior)) + 
  geom_line() + ylab("escapement")
```

![](parameter-uncertainty_files/figure-html/unnamed-chunk-6-1.png)<!-- -->







...

Thus far these are stationary policies -- the policy is a function of the observed state, but given the observation the policy is fixed -- no learning takes place from these observations.  

Now we consider updating our belief over the models `qt` in light of each observation.  We begin with passive learning: we will update `qt` in response to each observation, but not take actions to actively 'explore' and accelerate learning.  



```r
grid_i <-function(x, grid){
  which.min( abs(x - grid)) 
}

bayes_update_qt <- function(qt, x_t, x_t1, a_t, transition){

  n_models <- length(transition)
  P <- vapply(1:n_models, function(m) transition[[m]][x_t,x_t1,a_t], numeric(1))
  qt * P / sum(qt * P)
  
}


## without harvest, system goes from state 3->3, (states[4] == 3), small r is more likely
qt1 <- bayes_update_qt(qt, 4, 4, 1, transition)
plot(seq_along(qt1), qt1)



## without harvest, system goes from state 3->6, middle r is likely
qt1 <- bayes_update_qt(qt, 4, 7, 1, transition)
plot(seq_along(qt1), qt1)
```

![](parameter-uncertainty_files/figure-html/unnamed-chunk-7-1.png)<!-- -->

```r
## with harvest=2, system goes from state 3->6, large r is most likely
qt1 <- bayes_update_qt(qt, 4, 7, 3, transition)
plot(seq_along(qt1), qt1)

## Compare actions that learn more or less? Determine optimal exploration, value aside?
```




Learn `r` passively over time:



```r
## Note that action & state are all given as index values to their respective vectors
learning_sim <- function(initial_state_index, transition, utility, 
                         qt = rep(1, length(transition)) / length(transition), 
                         Tmax = 20, true_transition){

  n_states <- dim(transition[[1]])[1]
  state <- numeric(Tmax)
  action <- numeric(Tmax)
  value <- numeric(Tmax)
  q <- array(NA, dim = c(Tmax, length(transition)))
  q[1,] <- qt
  state[1] <- initial_state_index
  
  
  for(t in 1:(Tmax-1)){
  
    ## Determine optimal action
    out <- value_iteration(transition, utility, q[t,], Tmax-t)
    ## Take proposed action, collect discounted reward
    action[t] <- out$policy[state[t]] 
    value[t] <- utility[state[t], action[t]] * discount^(t-1)
    ## draw new state based on true model probability
    prob <- true_transition[state[t], , action[t]]
    state[t+1] <- sample(1:n_states, 1, prob = prob)
    
    ## Update belief
    q[t+1,] <- bayes_update_qt(q[t,], state[t], state[t+1], action[t], transition)
    
    
  }

  # Final action and associated value
  # action[t+1] <- out$policy[state[t+1]] 
  # value[t+1] <- utility[state[t+1], action[t+1]]
    
  
  list(df = data.frame(time = 1:Tmax, state = state, action = action, value = value), posterior = q)
}
```




```r
true_i <- 3
ex <- learning_sim(initial_state_index = 7, 
                   transition, utility, 
                   qt = rep(1, length(transition)) / length(transition), 
                   Tmax = 50, 
                   true_transition = transition[[true_i]])
```


Illustrate change in prior over time as learning takes place:


```r
df <- data.frame(r = pars$r, 
                 quarter = ex$posterior[round(Tmax/4),], 
                 mid = ex$posterior[round(Tmax/2),], 
                 threequarter = ex$posterior[round(3*Tmax/4),], 
                 end = ex$posterior[Tmax,])
ggplot(df) + 
    geom_line(aes(r, quarter), alpha=0.25, lwd = 0.25) + 
    geom_line(aes(r, mid), alpha=0.5, lwd = 0.5) + 
    geom_line(aes(r, threequarter), alpha=0.75, lwd = 0.75) + 
    geom_line(aes(r, end), lwd = 1)
```

![](parameter-uncertainty_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


What does the policy appear to look like? We compare this to the optimal policy of the true model (see `certain_low`, above).  


```r
# (note these are really index values)
ggplot(ex$df, aes(state, state - action)) + 
  geom_point() +
  geom_line(data = data.frame(state = 1:length(states), action = certain_low$policy))
```

![](parameter-uncertainty_files/figure-html/unnamed-chunk-11-1.png)<!-- -->

We can also see how this policy is applied over time for the realized sequence of growth shocks:



```r
ggplot(ex$df) +  geom_line(aes(time, state)) + geom_line(aes(time, action), col = "red")
```

![](parameter-uncertainty_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

```r
sum(ex$df$value)
```

```
## [1] 69.15797
```



A second example, with much higher r as the true model:



```r
true_i <- 10
ex <- learning_sim(initial_state_index = length(states), 
                   transition, utility, 
                   qt = rep(1, length(transition)) / length(transition), 
                   Tmax = 50, 
                   true_transition = transition[[true_i]])
```



```r
ggplot(ex$df, aes(state, state - action)) + 
  geom_point() +
  geom_line(data = data.frame(state = 1:length(states), action = certain_high$policy))
```

![](parameter-uncertainty_files/figure-html/unnamed-chunk-14-1.png)<!-- -->


<!--
Can we invert this to determine a model prior for which the observed action was optimal?  No, since this is many-to-one, e.g. always at least one model for which the action is optimal.  
-->
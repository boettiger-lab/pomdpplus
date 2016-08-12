



```r
library("appl")
library("pomdpplus")
library("ggplot2")
library("tidyr")
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
knitr::opts_chunk$set(cache = TRUE)
```



## Problem definition


```r
states <- 0:20
actions <- states
obs <- states

sigma_g <- sqrt(log(1 + 0.1 / 6)) # Scale the log-standard-deviation to result in similar variance to a uniform distribution of width 0.5
sigma_m <- sigma_g 

reward_fn <- function(x,h) pmin(x,h)
discount <- 0.95


K1 <- function(x, h, r = 0.5, K = 10){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}


K2 <- function(x, h, r = 0.5, K = 15){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}

Ks <- list(K1, K2)
```



```r
models <- lapply(Ks, function(f) fisheries_matrices(states, actions, obs, reward_fn, f, sigma_g, sigma_m))
```


## Planning solution

Compute Q matrices using pomdpsol for each model (intensive).  Since we do not specify a prior belief over states, uses default assumption of uniform belief over states.


```r
alphas <- sarsop_plus(models, discount, precision = .1)
```

```
## load time: 0.04 sec, init time: 0.15 sec, run time: 0.52 sec, final precision: 0.0958766 end_condition:   target precision reached
```

```
## load time: 0.04 sec, init time: 0.15 sec, run time: 23.55 sec, final precision: 0.0999691 end_condition:   target precision reached
```


We can compare results for a different priors over states.  For simplicity of interpretation, we assume the model is known to be model 2 (model prior `(0,1)`)
As expected, the policy is much more conservative when prior belief is lower!  


```r
low <-  compute_plus_policy(alphas, models, c(0, 1), models[[2]]$observation[,4,1])
ave <-  compute_plus_policy(alphas, models, c(0, 1), models[[2]]$observation[,10,1])
unif <- compute_plus_policy(alphas, models, c(0, 1))
high <- compute_plus_policy(alphas, models, c(0, 1), models[[2]]$observation[,15,1])
df <- dplyr::bind_rows(low, ave, unif, high, .id = "prior")

ggplot(df, aes(states[state], states[state] - actions[policy], col = prior, pch = prior)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_line()
```

![](pomdp-learning_files/figure-html/unnamed-chunk-5-1.png)<!-- -->


Use alphas to compute policy given model priors, for comparison:


```r
compare_policies <- function(alphas, models){
  low <-  compute_plus_policy(alphas, models, c(1, 0))
  unif <- compute_plus_policy(alphas, models, c(1/2, 1/2))
  high <- compute_plus_policy(alphas, models, c(0, 1))
  dplyr::bind_rows(low, unif, high, .id = "prior")
}


df <- compare_policies(alphas, models)
ggplot(df, aes(states[state], states[state] - actions[policy], col = prior, pch = prior)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_line()
```

![](pomdp-learning_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


```r
set.seed(1234)
out <- sim_plus(models = models, discount = discount,
                x0 = 2, a0 = 1, Tmax = 100, 
                true_model = models[[2]], 
                alphas = alphas)
```


```r
out$df %>% 
  dplyr::select(-value) %>% 
  tidyr::gather(variable, stock, -time) %>% 
  ggplot(aes(time, stock, color = variable)) + geom_line()
```

![](pomdp-learning_files/figure-html/unnamed-chunk-8-1.png)<!-- -->

Belief is never stationary (examining belief distributions after 75 iterations):


```r
Tmax <-length(out$state_posterior[,1])
out$state_posterior %>% data.frame(time = 1:Tmax) %>% 
  tidyr::gather(state, probability, -time, factor_key =TRUE) %>% 
  dplyr::mutate(state = as.numeric(state)) %>% 
  dplyr::filter(time > 75) %>% 
  ggplot(aes(state, probability, group = time, alpha = time)) + geom_line()
```

![](pomdp-learning_files/figure-html/unnamed-chunk-9-1.png)<!-- -->

Whereas model posterior converges more quickly to the true model (examining first 15 probabilities already shows model 2 probability nearly 1)


```r
out$model_posterior %>% data.frame(time = 1:Tmax) %>% 
  tidyr::gather(model, probability, -time, factor_key =TRUE) %>% 
  dplyr::filter(time < 50) %>% 
  ggplot(aes(model, probability, group = time, alpha = time)) + geom_point()
```

![](pomdp-learning_files/figure-html/unnamed-chunk-10-1.png)<!-- -->


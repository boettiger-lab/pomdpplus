



```r
library("MDPtoolbox")
```

```
## Loading required package: Matrix
```

```
## Loading required package: linprog
```

```
## Loading required package: lpSolve
```

```r
library("appl")
library("ggplot2")
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


```r
source("pomdp_learning.R")
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


f1 <- function(x, h, r = 1, K = 15){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}

f2 <- function(x, h, r = 1, K = 10){
  s <- pmax(x - h, 0)
  s * exp(r * (1 - s / K) )
}
```



```r
m1 <- fisheries_matrices(states, actions, obs, reward_fn, f1, sigma_g, sigma_m)
m2 <- fisheries_matrices(states, actions, obs, reward_fn, f2, sigma_g, sigma_m)

models <- list(m1,m2)
model_prior <- c(0.5, 0.5)
```


## POMDP solution, model 1


```r
soln_1 <- pomdp_solve(m1$transition, m1$observation, m1$reward, discount, precision = 1)
```

```
## load time: 0.05 sec, init time: 0.15 sec, run time: 1.67 sec, final precision: 0.942936 end_condition:   target precision reached
```

```r
soln_2 <- pomdp_solve(m2$transition, m2$observation, m2$reward, discount, precision = 1)
```

```
## load time: 0.04 sec, init time: 0.1 sec, run time: 0.29 sec, final precision: 0.954564 end_condition:   target precision reached
```



```r
rbind(data.frame(model = "m1", soln_1),
      data.frame(model = "m2", soln_2)) %>%
ggplot(aes(states[state], states[state] - actions[policy], col = model)) + geom_point()
```

![](pomdp-learning_files/figure-html/unnamed-chunk-6-1.png)<!-- -->


## Planning solution


```r
soln <- pomdp_planning(models, discount, model_prior, verbose = TRUE, mc.cores = 1L, precision = 1)
```

```
## load time: 0.06 sec, init time: 0.16 sec, run time: 1.54 sec, final precision: 0.942936 end_condition:   target precision reached
```

```
## load time: 0.04 sec, init time: 0.11 sec, run time: 0.28 sec, final precision: 0.954564 end_condition:   target precision reached
```



```r
df <- 
  rbind(data.frame(model = "m1", soln_1),
      data.frame(model = "m2", soln_2),
      data.frame(model = "unif", soln))

ggplot(df, aes(states[state], states[state] - actions[policy], col = model, pch = model)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_line() + 
  ylim(0,15)
```

```
## Warning: Removed 2 rows containing missing values (geom_point).
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![](pomdp-learning_files/figure-html/unnamed-chunk-8-1.png)<!-- -->


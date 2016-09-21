
# Setup


```r
library("mdplearning")
library("appl")
library("pomdpplus")

library("ggplot2")
library("tidyr")
library("purrr")
library("dplyr")

library("seewave") ## For KL divergence only

knitr::opts_chunk$set(cache = TRUE, message=FALSE)
theme_set(theme_bw())
```

(Scaled data created from RAM data via script in `mdplearning/data-raw/scaled_data.R`)




```r
library("nimble")
data("scaled_data")
ricker_code <- nimbleCode({
  x[1] <- x0
  for(t in 1:(N-1)){
    s[t] <-  x[t] - min(x[t], a[t])
    mu[t] <- log(s[t])  + r * (1 - s[t] / K) 
    x[t+1] ~ dlnorm(mu[t], sd = sigma) 
  }
})

ricker_model <- nimbleModel(code = ricker_code, 
                            constants = list(N = length(scaled_data$a), a = scaled_data$a),
                            inits =  list(K = 1, r = 0.1, sigma = 0.1, x0 = scaled_data$y[1]), 
                            data = data.frame(x = scaled_data$y))

mloglik <- function(pars){
    if(any(pars < 0 )) return(1e12)
    ricker_model$r <- pars[1]
    ricker_model$K <- pars[2]
    ricker_model$sigma <- pars[3]
    - calculate(ricker_model)
}

ricker_fit <- optim(c(r = .1, K = 1, sigma = 0.1), mloglik, control = list(maxit = 20000))
ricker_fit$par
```



## Problem definition

(Code to create the logged alpha vectors; cached & not evaluated, very slow)


```r
log_dir = "."

## Parameters from nimble's maximum likelihood estimate
K = 0.9903371
r = 0.05699246
sigma_g = 0.01720091

states <- seq(0,1.2, length=100) # Vector of all possible states
actions <- seq(0,.8, length=100)   # Vector of actions: harvest
obs <- states
reward_fn <- function(x,h) pmin(x,h)

## Create a data frame of all parameter combinations to be run, along with fixed parameters
vars <- expand.grid(r = seq(0.05, 0.3, by =0.05), sigma_m = c(0.1, 0.3, 0.6))
fixed <- data.frame(model = "ricker", sigma_g = sigma_g, discount = 0.99, 
                    precision = 0.0000001, K = K, C = NA,  max_state = max(states),
                    max_obs = max(obs), max_action = max(actions), min_state = min(states),
                    min_obs = min(obs), min_action = min(actions))
models <- data.frame(vars, fixed)


## Detect available memory (linux servers only)
memory <- round(0.95 * as.numeric(gsub(".* (\\d+) .*", "\\1", system("cat /proc/meminfo", intern=TRUE)[1])) / 1000)

## Compute alphas for the above examples
for(i in 1:dim(models)[1]) {
## Select the model
  f <- switch(models[i, "model"], 
    allen = appl:::allen(models[i, "r"], models[i, "K"], models[i, "C"]),
    ricker = appl:::ricker(models[i, "r"], models[i, "K"])
  )
## Determine the matrices
  m <- appl::fisheries_matrices(states, actions, obs, reward_fn, f = f, 
                          sigma_g = models[i, "sigma_g"], sigma_m  = models[i, "sigma_m"])
## record data for the log
  log_data <- data.frame(model = models[i, "model"], r = models[i, "r"], K  = models[i, "K"], 
                         C = models[i, "C"], sigma_g = models[i, "sigma_g"], sigma_m = models[i, "sigma_m"],
                         memory = memory)
## run sarsop
  alpha <- appl::sarsop(m$transition, m$observation, m$reward, 
                        discount = models[i, "discount"], 
                        precision = models[i, "precision"], memory = memory,
                        log_dir = log_dir, log_data = log_data)

}
```

Identify available solutions in the log that match the desired parameters


```r
log_dir <- "https://raw.githubusercontent.com/cboettig/pomdp-solutions-library/master/library"

## 100 states, 30 GB solution
meta <- appl::meta_from_log(data.frame(model ="ricker", n_states = 100, sigma_m = 0.3), log_dir)
meta <- data.frame(meta, max_state = 1.2, max_obs = 1.2, max_action = 0.8)
meta <- meta %>% filter(date > as.Date("2016-09-10"))

## 50 states, 60 GB solution
#meta <- appl::meta_from_log(data.frame(model ="ricker", n_states = 50, sigma_m = 0.3), log_dir)
#meta <- data.frame(meta, max_state = 1.2, max_obs = 1.2, max_action = 0.8)

knitr::kable(meta)
```


```
id                                      load_time_sec   init_time_sec   run_time_sec   final_precision  end_condition    n_states   n_obs   n_actions   discount  date                  model        r           K    C     sigma_g   sigma_m   memory   max_state   max_obs   max_action
-------------------------------------  --------------  --------------  -------------  ----------------  --------------  ---------  ------  ----------  ---------  --------------------  -------  -----  ----------  ---  ----------  --------  -------  ----------  --------  -----------
a333a384-063c-4b4b-a4e6-5589cce2d562             3.52            7.41         663.76         0.0195426  NA                    100     100         100       0.99  2016-09-11 08:13:32   ricker    0.05   0.9903371   NA   0.0172009       0.3    29340         1.2       1.2          0.8
917bf4bc-7388-4d17-b7d1-d1a8d10c5cf2             3.54           19.58         336.82         0.0487519  NA                    100     100         100       0.99  2016-09-11 08:27:08   ricker    0.10   0.9903371   NA   0.0172009       0.3    29340         1.2       1.2          0.8
09dcf263-47fb-46e4-bf80-6297282ead37             3.52           16.32         346.14         0.0967042  NA                    100     100         100       0.99  2016-09-11 08:40:45   ricker    0.15   0.9903371   NA   0.0172009       0.3    29340         1.2       1.2          0.8
d05f31ac-3af0-4871-a26f-420c96cabbc9             3.54           55.88         427.31         0.1281140  NA                    100     100         100       0.99  2016-09-11 08:55:29   ricker    0.20   0.9903371   NA   0.0172009       0.3    29340         1.2       1.2          0.8
bb9345fd-0568-430d-93d2-44c969178da0             3.54           93.76         486.32         0.1613310  NA                    100     100         100       0.99  2016-09-11 09:11:16   ricker    0.25   0.9903371   NA   0.0172009       0.3    29340         1.2       1.2          0.8
6987a356-484d-4b02-839d-e3bcd613ac33             3.59          118.60         508.64         0.2531460  NA                    100     100         100       0.99  2016-09-11 09:27:33   ricker    0.30   0.9903371   NA   0.0172009       0.3    29340         1.2       1.2          0.8
```

Read in the POMDP problem specification from the log


```r
setup <- meta[1,]
states <- seq(0, setup$max_state, length=setup$n_states) # Vector of all possible states
actions <- seq(0, setup$max_action, length=setup$n_actions)   # Vector of actions: harvest
obs <- states
sigma_g <- setup$sigma_g
sigma_m <- setup$sigma_m
reward_fn <- function(x,h) pmin(x,h)
discount <- setup$discount 
models <- models_from_log(meta, reward_fn)
alphas <- alphas_from_log(meta, log_dir)
```


reformat model solutions for use by the MDP functions as well:


```r
transitions <- lapply(models, `[[`, "transition")
reward <- models[[1]]$reward
observation <- models[[1]]$observation
```

--------------------

# Verfication & Validation

## Examine the policies from POMDP/PLUS solutions

Compute the deterministic optimum solution:


```r
f <- f_from_log(meta)[[1]]
S_star <- optimize(function(x) x / discount - f(x,0), c(min(states),max(states)))$minimum
h <- pmax(states - S_star,  0)
policy <- sapply(h, function(h) which.min((abs(h - actions))))
det <- data.frame(policy, value = 1:length(states), state = 1:length(states))
```


Compare a uniform prior to individial cases:


```r
low <-  compute_plus_policy(alphas, models, c(1,0,0,0,0,0))
unif <- compute_plus_policy(alphas, models) # e.g. 'planning only'
high <-  compute_plus_policy(alphas, models, c(0, 0, 0, 0, 0, 1))
df <- dplyr::bind_rows(low = low, unif = unif, high = high, det = det, .id = "prior")

ggplot(df, aes(states[state], states[state] - actions[policy], col = prior, pch = prior)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_line()
```

![](tuna-pomdp_files/figure-html/unnamed-chunk-8-1.png)<!-- -->



------------

# Analysis

## Hindcast: Historical catch and stock


```r
set.seed(123)
data("scaled_data")
y <- sapply(scaled_data$y, function(y) which.min(abs(states - y)))
a <- sapply(scaled_data$a, function(a) which.min(abs(actions - a)))
Tmax <- length(y)

data("bluefin_tuna")
```

```
## Warning in data("bluefin_tuna"): data set 'bluefin_tuna' not found
```

```r
to_mt <- max(bluefin_tuna$total) # 1178363 # scaling factor for data
states_mt <- to_mt * states
actions_mt <- to_mt * actions
year <- 1952:2009
future <- 2009:2067
```


### Compute PLUS optimum vs historical data:



```r
plus_hindcast <- compare_plus(models = models, discount = discount,
                    obs = y, action = a, alphas = alphas)
```

### Compare MDP optimum vs historical data: 


```r
mdp_hindcast <- mdp_historical(transitions, reward, discount, state = y, action = a)
```


### Merge and plot resulting optimal solutions


```r
left_join(rename(plus_hindcast$df, plus = optimal, state = obs),  
          rename(mdp_hindcast$df, mdp = optimal)) %>%
mutate("actual catch" = actions_mt[action], "estimated stock" = states_mt[state], 
       plus = actions_mt[plus], mdp = actions_mt[mdp], time = year[time]) %>%
       select(-state, -action) %>%
gather(variable, stock, -time) %>% 
ggplot(aes(time, stock, color = variable)) + geom_line(lwd=1) #  + geom_point()
```

```
## Warning: Removed 2 rows containing missing values (geom_path).
```

![](tuna-pomdp_files/figure-html/unnamed-chunk-12-1.png)<!-- -->

## Compare rates of learning


```r
# delta function for true model distribution
h_star = array(0,dim = length(models)) 
h_star[1] = 1
## Fn for the base-2 KL divergence from true model, in a friendly format
kl2 <- function(value) seewave::kl.dist(value, h_star, base = 2)[[2]]

bind_rows(plus = plus_hindcast$model_posterior,
          mdp = mdp_hindcast$posterior, 
          .id = "method") %>%
mutate(time = year[rep(1:Tmax,2)], rep = 1) %>%
gather(model, value, -time, -rep, -method) %>%
group_by(time, rep, method) %>% 
summarise(kl = kl2(value)) %>%

ggplot(aes(time, kl, col = method)) + 
stat_summary(geom="line", fun.y = mean, lwd = 1)
```

```
## Warning: Removed 2 rows containing non-finite values (stat_summary).
```

![](tuna-pomdp_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

### Final beliefs

Show the final belief over models for pomdp and mdp:


```r
barplot(as.numeric(plus_hindcast$model_posterior[Tmax,]))
```

![](tuna-pomdp_files/figure-html/unnamed-chunk-14-1.png)<!-- -->



```r
barplot(as.numeric(mdp_hindcast$posterior[Tmax,]))
```

![](tuna-pomdp_files/figure-html/unnamed-chunk-15-1.png)<!-- -->



## Forecast simulations under PLUS and MDP-learning

All forecasts start from final stock, go forward an equal length of time:


```r
#x0 <- which.min(abs(1 - states)) # Initial stock, for hindcasts
#x0 <- y[length(y)] # Final stock, e.g. forcasts, = 9

## too low a final value causes MDP to crash too soon
x0 <- 12
Tmax <- length(y)
set.seed(123)
```

Note also that forecasts start with the prior belief over states and prior belief over models that was determined from the historical data.  


```r
plus_forecast <- 
plus_replicate(50, 
               sim_plus(models = models, discount = discount,
                        model_prior = as.numeric(plus_hindcast$model_posterior[length(y), ]),
                        state_prior = as.numeric(plus_hindcast$state_posterior[length(y), ]),
                        x0 = x0, Tmax = Tmax, true_model = models[[1]], alphas = alphas), 
               mc.cores = parallel::detectCores())
```

We simulate replicates under MDP learning (with observation uncertainty):


```r
set.seed(123)
mdp_forecast <- 
plus_replicate(50, 
               mdp_learning(transition = transitions, reward = models[[1]]$reward, 
                            model_prior = as.numeric(mdp_hindcast$posterior[length(y),]),
                            discount = discount, x0 = x0,  Tmax = Tmax,
                            true_transition = transitions[[1]], 
                            observation = models[[1]]$observation),
               mc.cores = parallel::detectCores())
```

## Compare forecasts


```r
historical <- bluefin_tuna[c("tsyear", "total", "catch_landings")] %>% 
  rename(time = tsyear, state = total, action = catch_landings) %>% 
  mutate(method = "historical")
```

```
## Warning: failed to assign NativeSymbolInfo for env since env is already
## defined in the 'lazyeval' namespace
```

```r
bind_rows(plus = plus_forecast$df, 
          mdp = mdp_forecast$df,
          .id = "method")  %>% 
select(-value, -obs) %>% 
mutate(state = states_mt[state], action = actions_mt[action], time = future[time]) %>% 
bind_rows(historical) %>%
rename("catch (MT)" = action, "stock (MT)" = state) %>%  
gather(variable, stock, -time, -rep, -method) %>%
ggplot(aes(time, stock)) + 
  #geom_line(aes(group = interaction(rep,method), color = method), alpha=0.1) +
  stat_summary(aes(color = method), geom="line", fun.y = mean, lwd=1) +
  stat_summary(aes(fill = method), geom="ribbon", fun.data = mean_sdl, fun.args = list(mult=1), alpha = 0.25) + 
  facet_wrap(~variable, ncol = 1, scales = "free_y")
```

![](tuna-pomdp_files/figure-html/unnamed-chunk-19-1.png)<!-- -->


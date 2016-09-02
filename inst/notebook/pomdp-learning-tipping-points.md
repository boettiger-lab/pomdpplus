



```r
library("appl")
library("pomdpplus")
library("ggplot2")
library("tidyr")
library("purrr")
library("dplyr")
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:purrr':
## 
##     contains, order_by
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
log_dir <- "https://raw.githubusercontent.com/cboettig/pomdp-solutions-library/master/library"
meta <- appl::meta_from_log(data.frame(model = "allen", r = 0.5, K = 30), log_dir) 

meta <- meta[1:5,]

meta
```

```
##                                      id load_time_sec init_time_sec
## 30 4ec09f2e-1e28-4ea0-8751-4d34d5387e73          0.31         52.92
## 31 46ee73b8-5abf-4b6c-bd1e-39325b31292a          0.31         51.77
## 32 4671da66-c97d-4aca-b549-e69ef7f734aa          0.32         50.84
## 33 68aea28b-413e-4b05-b452-d62c1c948204          0.31         50.61
## 34 1e0c4970-ee74-424d-81d5-675f416799b3          0.32         46.73
##    run_time_sec final_precision end_condition n_states n_obs n_actions
## 30      1433.61        14.29720          <NA>       41    41        41
## 31      1699.65        13.83400          <NA>       41    41        41
## 32      1488.34        10.88400          <NA>       41    41        41
## 33      1513.32        11.86770          <NA>       41    41        41
## 34      5098.18         9.60597          <NA>       41    41        41
##    discount                date model   r  K  C sigma_g sigma_m memory
## 30     0.99 2016-09-01 05:48:02 allen 0.5 30  0     0.1    0.25  15612
## 31     0.99 2016-09-01 06:16:33 allen 0.5 30  3     0.1    0.25  15612
## 32     0.99 2016-09-01 06:41:33 allen 0.5 30  6     0.1    0.25  15612
## 33     0.99 2016-09-01 07:06:58 allen 0.5 30  9     0.1    0.25  15612
## 34     0.99 2016-09-01 08:32:08 allen 0.5 30 12     0.1    0.25  15612
```


## Import parameters from log



```r
setup <- meta[1,]

states <- 0:(setup$n_states - 1)
actions <- states
obs <- states

sigma_g <- setup$sigma_g
sigma_m <- setup$sigma_m

reward_fn <- function(x,h) pmin(x,h)
discount <- setup$discount 

models <- models_from_log(meta, reward_fn)
alphas <- alphas_from_log(meta, log_dir)
```


Policy based on a uniform prior belief over the models:  


```r
C0 <- compute_plus_policy(alphas, models, model_prior  = c(1,0,0,0,0))
C5 <- compute_plus_policy(alphas, models, model_prior  = c(0,1,0,0,0))
C10 <- compute_plus_policy(alphas, models, model_prior = c(0,0,1,0,0))
C15 <- compute_plus_policy(alphas, models, model_prior = c(0,0,0,1,0))
C20 <- compute_plus_policy(alphas, models, model_prior = c(0,0,0,0,1))
unif <- compute_plus_policy(alphas, models)

df <- dplyr::bind_rows(C0, C5, C10, C15, unif, .id = "prior")

ggplot(df, aes(states[state], states[state] - actions[policy], col = prior, pch = prior)) + 
  geom_point(alpha = 0.5, size = 3) + 
  geom_line()
```

![](pomdp-learning-tipping-points_files/figure-html/unnamed-chunk-4-1.png)<!-- -->



```r
set.seed(1234)
out <- sim_plus(models = models, discount = discount,
                x0 = 20, a0 = 1, Tmax = 100, 
                true_model = models[[2]], 
                alphas = alphas)


out$df %>% 
  dplyr::select(-value) %>% 
  tidyr::gather(variable, stock, -time) %>% 
  ggplot(aes(time, stock, color = variable)) + geom_line()  + geom_point()
```

![](pomdp-learning-tipping-points_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

Evolution of the belief state:


```r
Tmax <-length(out$state_posterior[,1])
out$state_posterior %>% data.frame(time = 1:Tmax) %>% 
  tidyr::gather(state, probability, -time, factor_key =TRUE) %>% 
  dplyr::mutate(state = as.numeric(state)) %>% 
  ggplot(aes(state, probability, group = time, alpha = time)) + geom_line()
```

![](pomdp-learning-tipping-points_files/figure-html/unnamed-chunk-6-1.png)<!-- -->




```r
out$model_posterior %>% data.frame(time = 1:Tmax) %>% 
  tidyr::gather(model, probability, -time, factor_key =TRUE) %>% 
  ggplot(aes(model, probability, group = time, alpha = time)) + geom_point()
```

![](pomdp-learning-tipping-points_files/figure-html/unnamed-chunk-7-1.png)<!-- -->



Replicates: 


```r
sims <- 
purrr::map_df(1:25, 
       function(i) sim_plus(models = models, discount = discount,
                x0 = 20, a0 = 1, Tmax = 100, 
                true_model = models[[3]], 
                alphas = alphas)$df,
  .id = "rep")
```



```r
sims %>% 
  ggplot(aes(time, state, group = rep)) + geom_line(alpha = 0.5)
```

![](pomdp-learning-tipping-points_files/figure-html/unnamed-chunk-9-1.png)<!-- -->


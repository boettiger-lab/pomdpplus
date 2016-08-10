# parameter-uncertainty-POMDP
Milad Memarzadeh
7/25/2016  

Num_s = 20; sigma_g = 0.2, sigma_m = 0.2; Num_a = 21; Num_z = 20

r \in {0.4,1,2}, K = 18

K \in {10,14,18}, r = 1

## Policy

### MDP

![](parameter-uncertainty_files/figure-html/MDP_Pol_r.png)

![](parameter-uncertainty_files/figure-html/MDP_Pol_k.png)


### POMDP

![](parameter-uncertainty_files/figure-html/Pol_r.png)

![](parameter-uncertainty_files/figure-html/Pol_K.png)



## Value

### MDP

![](parameter-uncertainty_files/figure-html/MDP_Val_r.png)

![](parameter-uncertainty_files/figure-html/MDP_Val_K.png)


### POMDP

![](parameter-uncertainty_files/figure-html/Val_r.png)

![](parameter-uncertainty_files/figure-html/Val_K.png)

## relative Values

### r
![](parameter-uncertainty_files/figure-html/relV_r.png)

### K

![](parameter-uncertainty_files/figure-html/relV_K.png)


## Forward Simulations

### Model with K = 10, r = 1

#### Belief time 1 to 10
![](parameter-uncertainty_files/figure-html/k2_b_10.png)

#### Belief time 1 to 100 by 25
![](parameter-uncertainty_files/figure-html/k2_b_100.png)

#### trajectories
![](parameter-uncertainty_files/figure-html/k2_traj.png)


### Model with K = 18, r = 1

#### Belief time 1 to 10
![](parameter-uncertainty_files/figure-html/k10_b_10.png)

#### Belief time 1 to 100 by 25
![](parameter-uncertainty_files/figure-html/k10_b_100.png)

#### trajectories
![](parameter-uncertainty_files/figure-html/k10_traj.png)


### Model with K = 18, r = 2

#### Belief time 1 to 10
![](parameter-uncertainty_files/figure-html/r10_b_10.png)

#### Belief time 1 to 100 by 25
![](parameter-uncertainty_files/figure-html/r10_b_100.png)

#### trajectories
![](parameter-uncertainty_files/figure-html/r10_traj.png)


### Model with K = 10, r = 1.6

#### Belief time 1 to 10
![](parameter-uncertainty_files/figure-html/r8_b_10.png)

#### Belief time 1 to 100 by 25
![](parameter-uncertainty_files/figure-html/r8_b_100.png)

#### trajectories
![](parameter-uncertainty_files/figure-html/r8_traj.png)

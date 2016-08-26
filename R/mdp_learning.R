
#' compute mdp policy
#'
#' @param transition list of transition matrices, one per model
#' @param reward the utility matrix U(x,a) of being at state x and taking action a
#' @param discount the discount factor (1 is no discounting)
#' @param model_prior the prior belief over models, a numeric of length(transitions). Uniform by default
#' @param max_iter maximum number of iterations to perform
#' @param epsilon convergence tolerance
#' @param type consider converged when policy converges or when value converges?
#'
#' @return a data.frame with the optimal policy and (discounted) value associated with each state
#' @export
#'
#' @examples
#' source(system.file("examples/K_models.R", package="pomdpplus"))
#' transition <- lapply(models, `[[`, "transition")
#' reward <- models[[1]][["reward"]]
#' df <- compute_mdp_policy(transition, reward, discount)
#' plot(df$state, df$state - df$policy, xlab = "stock", ylab="escapement")
compute_mdp_policy <- function(transition, reward, discount,
                               model_prior = rep(1, length(transition))/length(transition),
                               max_iter = 500, epsilon = 1e-5, type = c("policy iteration", "value iteration", "finite time")){

  type <- match.arg(type)

  n_models <- length(transition)
  n_states <- dim(transition[[1]])[1]
  n_actions <- dim(transition[[1]])[3]
  next_value <- numeric(n_states)
  next_policy <- numeric(n_states)
  V_model <- array(dim=c(n_states, n_models))
  converged <- FALSE
  t <- 1

  while(t < max_iter && converged == FALSE){
    Q <- array(0, dim = c(n_states, n_actions))
    for (i in 1:n_actions) {
      for(j in 1:n_models){
        V_model[,j] <- transition[[j]][,,i] %*% next_value
      }
      Q[,i] <- reward[, i] + discount * V_model %*% model_prior
    }
    value <- apply(Q, 1, max)
    policy <- apply(Q, 1, which.max)


    if(type == "value iteration"){
      if( sum( abs(value - next_value) ) < epsilon ){
        converged <- TRUE
      }
    } else if(type == "policy iteration"){
      if( sum( abs(policy - next_policy) ) < epsilon ){
        converged <- TRUE
      }
    }

    next_value <- value
    next_policy <- policy
    t <- t+1
    if(t == max_iter)
      message("Note: max number of iterations reached")
  }
  data.frame(state = 1:n_states, policy, value)
}




#' mdp learning
#'
#' @inheritParams compute_mdp_policy
#' @param x0 initial state
#' @param Tmax length of time to simulate
#' @param true_transition actual transition used to drive simulation
#' @return a list, containing: data frame "df" with the state, action and a value at each time step in the simulation,
#' and an array "posterior", in which the t'th row shows the belief state at time t.
#' @export
#'
#' @examples
#' source(system.file("examples/K_models.R", package="pomdpplus"))
#' transition <- lapply(models, `[[`, "transition")
#' reward <- models[[1]][["reward"]]
#'
#' ## example where true model is model 1
#' out <- mdp_learning(transition, reward, discount, x0 = 10,
#'                     Tmax = 20, true_transition = transition[[1]])
#' ## Did we learn which one was the true model?
#' out$posterior[20,]
mdp_learning <- function(transition, reward, discount,
                         model_prior = rep(1, length(transition)) / length(transition),
                         x0, Tmax = 20, true_transition, max_iter = 500, epsilon = 1e-5,
                         type = c("policy iteration", "value iteration", "finite time")){
  type <- match.arg(type)
  n_states <- dim(transition[[1]])[1]
  state <- numeric(Tmax)
  action <- numeric(Tmax)
  value <- numeric(Tmax)
  q <- array(NA, dim = c(Tmax, length(transition)))
  q[1,] <- model_prior
  state[1] <- x0

  for(t in 1:(Tmax-1)){
    if(type == "finite time") max_iter <- Tmax - t+1
    out <- compute_mdp_policy(transition, reward, discount, q[t,], max_iter = max_iter, epsilon = epsilon, type = type)
    action[t] <- out$policy[state[t]]
    value[t] <- reward[state[t], action[t]] * discount^(t-1)
    prob <- true_transition[state[t], , action[t]]
    state[t+1] <- sample(1:n_states, 1, prob = prob)
    q[t+1,] <- bayes_update_model_belief(q[t,], state[t], state[t+1], action[t], transition)
  }

  list(df = data.frame(time = 1:Tmax, state, action, value), posterior = q)
}

bayes_update_model_belief <- function(model_prior, x_t, x_t1, a_t, transition){
  n_models <- length(transition)
  P <- vapply(1:n_models, function(m) transition[[m]][x_t,x_t1,a_t], numeric(1))
  model_prior * P / sum(model_prior * P)
}

#############################################################################################################################

#' mdp_policy_sim
#'
#' Simulate a (static) policy given the transition model
#' @inheritParams compute_mdp_policy
#' @inheritParams mdp_learning
#' @param transition a single transition matrix that wil lbe used for the simulation
#' @param policy the policy to apply in the simulation
#' @return a data.frame with the state, action, and value at each time t
#' @examples
#' ## Setup
#' source(system.file("examples/K_models.R", package="pomdpplus"))
#' transition <- lapply(models, `[[`, "transition")
#' reward <- models[[1]][["reward"]]
#'
#' ## Simulate how policy over model uncertainty performs when model 1 is true:
#' df <- compute_mdp_policy(transition, reward, discount)
#' out <- mdp_policy_sim(df$policy, transition[[1]], reward, discount, x0 = 10, Tmax = 20)
#' @export
mdp_policy_sim <- function(policy, transition, reward, discount, x0, Tmax){
  n_states <- dim(transition)[1]
  state <- action <- value <- numeric(Tmax)
  time <- 1:(Tmax-1)
  state[1] <- x0
  for(t in time){
    action[t] <- policy[state[t]]
    value[t] <- reward[state[t], action[t]] * discount^(t-1)
    prob <- transition[state[t], , action[t]]
    state[t+1] <- sample(1:n_states, 1, prob = prob)
  }
  data.frame(time = 1:Tmax, state, action, value)
}





#' mdp_value_of_policy
#'
#' Compute the expected net present (e.g. discounted) value of a (not-necessarily optimal) policy in a perfectly observed (MDP) system
#' @inheritParams compute_mdp_policy
#' @param policy the policy for which we want to determine the expected value
#' @return the expected net present value of the given policy, for each state
#' @details transition can be a single transition matrix or a list of transition matrices
#' @examples
#' source(system.file("examples/K_models.R", package="pomdpplus"))
#' transition <- lapply(models, `[[`, "transition")
#' reward <- models[[1]][["reward"]]
#' df <- compute_mdp_policy(transition, reward, discount)
#' v <- mdp_value_of_policy(df$policy, transition, reward, discount)
#' @export
mdp_value_of_policy <- function(policy, transition, reward, discount, model_prior = rep(1, length(transition))/length(transition), max_iter = 500, epsilon = 1e-5){
  if(is.array(transition)){
    transition <- list(transition)
  }
  n_models <- length(transition)
  n_states <- dim(transition[[1]])[1]
  n_actions <- dim(transition[[1]])[3]
  Vt <- numeric(n_states)
  next_value <- Vt
  V_model <- array(dim=c(n_states, n_models))
  converged <- FALSE
  t <- 1
  while(t < max_iter && converged == FALSE){
    Q <- array(0, dim = c(n_states, n_actions))
    for (i in 1:n_actions) {
      for(j in 1:n_models){
        V_model[,j] <- transition[[j]][,,i] %*% Vt
      }
      Q[,i] <- reward[, i] + discount * V_model %*% model_prior
    }

    for(i in 1:n_states)
      Vt[i] <- Q[i,policy[i]]

    if( sum( abs(Vt - next_value) ) < epsilon ){
      converged <- TRUE
    }
    next_value <- Vt
    t <- t + 1
    if(t == max_iter)
      message("Note: max number of iterations reached")
  }
  Vt
}


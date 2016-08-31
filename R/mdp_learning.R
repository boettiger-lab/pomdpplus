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


#' mdp sim
#'
#'
#' Simulate learning under the mdp policy
#' @details If no observation matrix is given, simulation is based on perfect observation.  Otherwise, simulation includes imperfect measurement
#' If a policy is given, this policy is used to determine the actions throughout the simulation and no learning takes place.
#' Otherwise, the simulation will compute the optimal (fully observed) MDP solution over the given transition models and model prior,
#' and continue to learn by updating the belief over models and the resulting optimal MDP policy.
#' @inheritParams compute_mdp_policy
#' @param x0 initial state
#' @param Tmax length of time to simulate
#' @param true_transition actual transition used to drive simulation. If a fixed policy is given, then true_transition = transition
#'  if it is not specified (i.e. avoids having to declare transition separately).
#' @param observation NULL by default, simulate perfect observations
#' @param a0 previous action before starting, irrelivant unless actions influence observations and true_observation is not null
#' @param policy a vector of length n_obs, whose i'th entry is the index of the optimal action given the system is in (observed) state i.
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
#' out <- mdp_sim(transition, reward, discount, x0 = 10,
#'                     Tmax = 20, true_transition = transition[[1]])
#' ## Did we learn which one was the true model?
#' out$posterior[20,]
#'
#' ## simulate a fixed policy
#' df <- compute_mdp_policy(transition, reward, discount, model_prior = c(0.5, 0.5))
#' out <- mdp_sim(transition[[1]], reward, discount, x0 = 10,
#'                Tmax = 20,
#'                policy = df$policy)
#'
#' ## Simulate MDP strategy under observation uncertainty
#'
#' out <- mdp_sim(transition = transition, reward, discount, x0 = 10,
#'                true_transition = transition[[1]],
#'                Tmax = 20, observation = models[[1]][["observation"]])
mdp_sim <- function(transition, reward, discount,
                    model_prior = NULL,
                    x0, Tmax = 20, true_transition = transition,
                    observation = NULL,
                    a0 = 1,
                    policy = NULL,
                    max_iter = 500, epsilon = 1e-5, type = c("policy iteration", "value iteration", "finite time")){
  type <- match.arg(type)

  n_states <- dim(true_transition)[1]
  state <- obs <- action <- value <- numeric(Tmax+1)

  if(is.null(policy)){
    if(is.null(model_prior))
      model_prior<- rep(1, length(transition)) / length(transition)
    q <- array(NA, dim = c(Tmax+2, length(transition)))
    q[2,] <- model_prior
  }

  state[2] <- x0
  action[1] <- a0

  time <- 2:(Tmax+1)
  for(t in time){

    ## if no policy is given, then we learn over the models and update policy each time
    if(is.null(policy)){
      if(type == "finite time") max_iter <- Tmax - t+1
      out <- compute_mdp_policy(transition, reward, discount, q[t,], max_iter = max_iter, epsilon = epsilon, type = type)
    } else {
      out <- list(policy = policy)
    }

    ## Use perfect observations unless observation matrix is given
    if(!is.null(observation)){
      obs[t] <- sample(1:dim(observation)[2], 1, prob = observation[state[t], , action[t-1]])
    } else{
      obs[t] <- state[t]
    }

    ## Select action, determine value, transition to next state
    action[t] <- out$policy[obs[t]]
    value[t] <- reward[state[t], action[t]] * discount^(t-1)
    prob <- true_transition[state[t], , action[t]]
    state[t+1] <- sample(1:n_states, 1, prob = prob)

    ## Update belief
    if(is.null(policy))
      q[t+1,] <- bayes_update_model_belief(q[t,], state[t], state[t+1], action[t], transition)
  }

  df = data.frame(time = 1:Tmax, state = state[time], obs = obs[time], action = action[time], value = value[time])
  if(is.null(policy))
    list(df = df, posterior = q[time,])
  else
    df
}

bayes_update_model_belief <- function(model_prior, x_t, x_t1, a_t, transition){
  n_models <- length(transition)
  P <- vapply(1:n_models, function(m) transition[[m]][x_t,x_t1,a_t], numeric(1))
  model_prior * P / sum(model_prior * P)
}



####################################


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


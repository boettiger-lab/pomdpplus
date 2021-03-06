% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/sarsop_plus.R
\name{sarsop_plus}
\alias{sarsop_plus}
\title{sarsop_plus}
\usage{
sarsop_plus(models, discount, state_prior = NULL, mc.cores = 1L,
  log_data = NULL, ...)
}
\arguments{
\item{models}{a list of lists, each of which gives the transition matrix, observation matrix and reward matrix for the model considered}

\item{discount}{discount rate}

\item{state_prior}{Prior belief that system is in state x_i and model m_i}

\item{mc.cores}{number of parallel cores to use}

\item{log_data}{a data frame with information to be logged, one row for each model. Leave NULL if not logging}

\item{...}{additional options to sarsop::sarsop, if alphas are not provided}
}
\value{
A list of alpha vector matrices
}
\description{
sarsop_plus
}
\examples{
\dontrun{
#' source(system.file("examples/K_models.R", package="pomdpplus"))
alphas <- sarsop_plus(models, discount, precision = 1)
unif <- compute_plus_policy(alphas, models, c(0.5, 0.5))
}
}

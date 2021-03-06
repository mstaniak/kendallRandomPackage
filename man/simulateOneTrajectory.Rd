% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulations_kendall_rw.R
\name{simulateOneTrajectory}
\alias{simulateOneTrajectory}
\title{Simulate one trajectory ofa Kendall random walk}
\usage{
simulateOneTrajectory(trajectory_length, step_dist, alpha,
  symmetric = FALSE, ...)
}
\arguments{
\item{trajectory_length}{Number of samples to simulate.}

\item{step_dist}{Function that returns random numbers from step distribution.}

\item{alpha}{Alpha parameter of the random walk}

\item{symmetric}{If TRUE, random walk on the whole real line will be simulated.}

\item{...}{Additional parameters to step distribution.}
}
\value{
Generated path of the random walk.
}
\description{
Simulate one trajectory ofa Kendall random walk
}

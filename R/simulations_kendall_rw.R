#' Helper function: min/max
#'
#' @param x numeric
#' @param y numeric
#'
#' @return min of arguments divided by max of arguments
#'

Z <- function(x, y){
  min(x, y)/max(x, y)
}

#' Helper function
#'
#' @param x numeric
#' @param y numeric
#' @param alpha numeric, parameter of Kendall random walk
#'
#' @return 0 or 1 with probability depending on x, y, alpha
#'

Qn <- function(x, y, alpha){
  p <- Z(abs(x), abs(y))^alpha
  sample(c(0,1), 1, prob=c(1-p, p))
}

#' Helper function
#'
#' @param x numeric
#' @param y numeric
#'
#' @return sign of the argument whose abs. val. is bigger
#'

U <- function(x, y){
  if(abs(x) > abs(y)) sign(x)
  else sign(y)
}

#' Simulate one trajectory ofa Kendall random walk
#'
#' @param trajectory_length Number of samples to simulate.
#' @param step_dist Function that returns random numbers from step distribution.
#' @param alpha Alpha parameter of the random walk
#' @param symmetric If TRUE, random walk on the whole real line will be simulated.
#' @param ... Additional parameters to step distribution.
#'
#' @return Generated path of the random walk.
#'
#'

simulateOneTrajectory <- function(trajectory_length, step_dist,
                                  alpha, symmetric = FALSE, ...) {
  Y <- step_dist(trajectory_length, ...)
  if(symmetric) {
    theta <- actuar::rpareto1(trajectory_length,
                              2*alpha,
                              1)*sample(c(-1, 1),
                                               trajectory_length,
                                               prob = c(0.5, 0.5),
                                               replace = TRUE)
  } else {
    theta <- actuar::rpareto1(trajectory_length, 2*alpha, 1)
  }

  Xn <- vector("numeric", trajectory_length )
  Xn[1:2] <- c(0, Y[1])

  for(i in 2:(trajectory_length - 1)) {
    Xn[i + 1] <- max(abs(Xn[i]),
                     abs(Y[i + 1]))*theta[i]^Qn(Xn[i],
                                                Y[i+1],
                                                alpha)*U(Xn[i],
                                                         Y[i + 1])
  }
  Xn
}

#' Simulate multiple trajectories of Kendall random walk
#'
#' Object returned by this has print and plot methods.
#'
#' @param number_of_simulations number of trajectories to generate.
#' @param trajectory_length length of trajectories.
#' @param step_dist function returning random numbers from step dist.
#' @param alpha alpha parameter.
#' @param symmetric If TRUE, random walk on the whole real line will be simulated.
#' @param ... parameters for step distribution.
#'
#' @return Object of class kendall_simulation. It is a list that consists of
#' \item{simulation}{Tibble with simulation id and simulated values,}
#' \item{step_distribution}{Name of the step distribution,}
#' \item{alpha}{Value of alpha parameter,}
#' \item{is_symmetric}{Logical value indicating if this is a symmetric Kendall R.W.}
#'
#' @export
#'
#' @examples
#' kendall_simulations <- simulate_kendall_rw(10, 1000, runif, 0.5)
#' # Kendall R.W. on positive half-line with uniform step distribution - 10 trajectories.
#' only_simulations <- kendall_simulations$simulation # tibble with simulated values
#' kendall_simulations
#'
#'

simulate_kendall_rw <- function(number_of_simulations, trajectory_length,
                                step_dist, alpha, symmetric = FALSE, ...) {

  listTmp <- as.list(1:number_of_simulations)
  tmp <- lapply(listTmp, function(l)
    simulateOneTrajectory(trajectory_length, step_dist,
                          alpha, symmetric, ...))

  result <- dplyr::bind_rows(lapply(listTmp,
              function(x) tibble::tibble(sim_id = x, sim = tmp[[x]])))
  simulation <- list(simulation = result,
                     step_distribution = as.character(substitute(step_dist))[1],
                     alpha = alpha,
                     is_symmetric = symmetric)
  class(simulation) <- c("kendall_simulation", "list")
  simulation
}


#' Transforming (scaling and shifting) Kendall random walks
#'
#' If one trajectory has length n, an_seq and bn_seq arguments should be sequnces of length n.
#' Object returned by this function has plot and print methods.
#'
#'
#' @param simulations tibble returned by simulation function
#' @param an_seq sequence that the trajectories will be multiplied by
#' @param bn_seq sequence that will be substracted from scaled trajectory
#'
#' @return List like in simulate_kendall_rw function after transforming trajectories.
#'
#' @export
#'
#' @examples
#' kendall_simulations <- simulate_kendall_rw(10, 1000, runif, 0.5)
#' scaled_kendall <- transform_kendall_rw(kendall_simulations, (1:1000)^(-2))
#' scaled_kendall # kendall random walked scaled by the sequence n^(-1/alpha)
#' scaled_data <- scaled_kendall$simulation # simulated values
#' plot(scaled_kendall)
#'

transform_kendall_rw <- function(simulations, an_seq = 1, bn_seq = 0) {
  sim_id <- sim <- NULL
  result <- dplyr::mutate(simulations$simulation,
                          simNo = as.factor(as.character(sim_id)))
  result <- dplyr::group_by(result, sim_id)
  result <- dplyr::mutate(result, sim = an_seq*sim - bn_seq)

  simulations$simulation <- result
  simulations
}

#' Generic function that draws simulated trajectories of Kendall random walk
#'
#' @param x object returned by normalising_sequences function.
#' @param max_x maximum value on x axis.
#' @param max_id Number of trajectories to plot. If NULL, all paths will be plotted.
#' @param level Y-axis value which will be marked (level to be crossed).
#' @param ... Other arguments
#'
#' @return ggplot2 object
#'
#' @export
#'

plot.kendall_simulation <- function(x, max_x = NULL, max_id = NULL, level = NULL, ...) {
  sim_id <- NULL
  n_sim <- max(unique(as.integer(as.character(x$simulation$sim_id))))
  trajectory_length <- dim(x$simulation)[1]/n_sim
  if(is.null(max_x)) max_x <- trajectory_length
  if(is.null(max_id)) max_id <- n_sim

  to_plot <- dplyr::ungroup(x$simulation)
  to_plot <- dplyr::mutate(to_plot, x = rep(1:trajectory_length, n_sim))
  to_plot <- dplyr::filter(to_plot, x <= max_x, sim_id <= max_id)
  plot_result <- ggplot2::ggplot(to_plot,
                                 ggplot2::aes_string(x = 'x', y = 'sim',
                                                     group = 'sim_id')) +
    ggplot2::geom_line() +
    ggplot2::theme_bw() +
    ggplot2::xlab("") +
    ggplot2::ylab("") +
    ggplot2::guides(color = "none")
  if(!is.null(level))
    plot_result <- plot_result + ggplot2::geom_abline(slope = 0,
                                                      intercept = level,
                                                      color = "red",
                                                      size = 1.2)
  plot_result
}

#' Generic function that prints information about simulated Kendall random walk
#'
#' @param x Object returned by simulate_kendall_rw or transform_kendall_rw function.
#' @param ... Other arguments.
#'
#' @export
#'

print.kendall_simulation <- function(x, ...) {
  simulations_number <- max(as.numeric(as.character(x$simulation$sim_id)))
  cat("Simulations of Kendall random walk \n")
  cat("Number of simulations: ", simulations_number, "\n")
  cat("Length of a single simulation: ", nrow(x$simulation)/simulations_number, "\n")
  cat("Step distribution: ", x$step_distribution, "\n")
  cat("Alpha parameter: ", x$alpha)
  invisible(x)
}

#' Calculate some characteristic for every simulated instance.
#'
#' @param simulations Object of class kendall_simulation.
#' @param summary_function Function that will be applied to each trajectory.
#'
#' @return data frame of class "kendall_summary".
#'
#' @export
#'

summarise_kendall_rw <- function(simulations, summary_function) {
  sim_id <- NULL
  sim <- NULL
  result <- dplyr::group_by(simulations$simulation, sim_id)
  result <- dplyr::summarise(result, aggregated = summary_function(sim))
  class(result) <- c("kendall_summary", class(result))
  result
}


#' Mutate each trajectory.
#'
#' @param simulations Object of class kendall_simulation.
#' @param mutate_function Function that will be applied to each trajectory.
#' @param df If TRUE, a d.f will be returned, if FALSE, simulations in the kendall_simulation
#'           object passed in simulations argument will be replaced by the result of mutate_function.
#'
#' @return data frame or a list (of class kendall_simulation)
#'
#' @export
#'

mutate_kendall_rw <- function(simulations, mutate_function, df = T) {
  tmp <- with(simulations$simulation,
           dplyr::group_by(simulations$simulation, sim_id) %>%
             dplyr::mutate(sim = mutate_function(sim)))
  if(df) {
    tmp
  } else {
    simulations$simulation <- tmp
    simulations
  }

}

#' Plot summary of Kendall random walk simulations.
#'
#' @param x Object of class kendall_summary
#' @param ... Optional arguments, currently ignored
#' @param type Type of the plot: density, histogram or boxplot
#'
#' @export
#'
#' @import ggplot2
#'
#' @return ggplot2 object
#'

plot.kendall_summary <- function(x, ..., type  = "density") {
  aggregated <- NULL

  plot <- ggplot(x, aes(x = aggregated))

  if(type == "density") {
    plot + geom_density()
  } else if(type == "histogram") {
    plot + geom_histogram()
  } else {
    plot + geom_boxplot()
  }
}

#' Print summary of Kendall random walk simulations.
#'
#' @param x Object of type kendall_summary
#' @param ... Optional parameters, currently ignored
#'
#' @export
#'

print.kendall_summary <- function(x, ...) {
  quantiles <- quantile(x$aggregated, na.rm = T,
                        probs = seq(0, 1, by = 0.1))
  labels <- names(quantiles)
  cat("Mean of the distribution: ", mean(x$aggregated, na.rm = T), "\n")
  cat("Standard deviation of the distribution: ", sd(x$aggregated, na.rm = T), "\n")
  cat("Maximum of the distribution: ", max(x$aggregated, na.rm = T))
  cat("Number of observations: ", nrow(x), "\n")
  cat("Quantiles of the distribution: \n")
  print(quantiles)
  invisible(x)
}

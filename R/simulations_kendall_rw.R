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
    theta <- EnvStats::rpareto(trajectory_length,
                               1,
                               2*alpha)*sample(c(-1, 1),
                                               trajectory_length,
                                               prob = c(0.5, 0.5),
                                               replace = TRUE)
  } else {
    theta <- EnvStats::rpareto(trajectory_length, 1, 2*alpha)
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
#' @param number_of_simulations number of trajectories to generate.
#' @param trajectory_length length of trajectories.
#' @param step_dist function returning random numbers from step dist.
#' @param alpha alpha parameter.
#' @param symmetric If TRUE, random walk on the whole real line will be simulated.
#' @param ... parameters for step distribution.
#'
#' @export
#'

simulate_kendall_rw <- function(number_of_simulations, trajectory_length,
                                step_dist, alpha, symmetric = FALSE, ...) {

  listTmp <- as.list(1:number_of_simulations)
  tmp <- lapply(listTmp, function(l)
    simulateOneTrajectory(trajectory_length, step_dist,
                          alpha, symmetric, ...))

  result <- dplyr::bind_rows(lapply(listTmp,
              function(x) tibble::tibble(simNo = x, sim = tmp[[x]])))
  simulation <- list(simulation = result,
                     step_distribution = as.character(substitute(step_dist))[1],
                     alpha = alpha,
                     is_symmetric = symmetric)
  class(simulation) <- c("kendall_simulation", "list")
  simulation
}


#' Normalising Kendall random walks
#'
#' @param simulations tibble returned by simulation function
#' @param an_seq sequence that the trajectories will be multiplied by
#' @param bn_seq sequence that will be substracted from scaled trajectory
#'
#' @return tibble
#'
#' @export
#'

transform_kendall_rw <- function(simulations, an_seq = 1, bn_seq = 0) {
  result <- mutate(simulations$simulation,
                   simNo = as.factor(as.character(simNo)))
  result <- group_by(result, simNo)
  result <- mutate(result, sim = an_seq*sim - bn_seq)

  simulations$simulation <- result
  simulations
}

#' Generic function that draws simulated trajectories of Kendall random walk
#'
#' @param x object returned by normalising_sequences function.
#' @param max_x maximum value on x axis.
#' @param level Y-axis value which will be marked (level to be crossed).
#' @param ... Other arguments
#'
#' @return ggplot2 object
#'
#' @export
#'

plot.kendall_simulation <- function(x, max_x = NULL, level = NULL, ...) {
  nSim <- max(unique(as.integer(as.character(x$simulation$simNo))))
  trajectory_length <- dim(x$simulation)[1]/nSim
  if(is.null(max_x)) max_x <- trajectory_length

  to_plot <- dplyr::ungroup(x$simulation)
  to_plot <- dplyr::mutate(to_plot, x = rep(1:trajectory_length, nSim))
  to_plot <- dplyr::filter(to_plot, x <= max_x)
  plot_result <- ggplot2::ggplot(to_plot, aes(x = x, y = sim, group = simNo)) +
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
  simulations_number <- max(as.numeric(as.character(x$simulation$simNo)))
  cat("Simulations of Kendall random walk \n")
  cat("Number of simulations: ", simulations_number, "\n")
  cat("Length of a single simulation: ", nrow(x$simulation)/simulations_number, "\n")
  cat("Step distribution: ", x$step_distribution, "\n")
  cat("Alpha parameter: ", x$alpha)
  invisible(x)
}

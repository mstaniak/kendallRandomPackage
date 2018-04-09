#' Helper function: min/max
#'
#' @param x numeric
#' @param y numeric
#'
#' @return min of arguments divided by max of arguments
#'

Z <- function(x,y){
  min(x,y)/max(x,y)
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
  if(abs(x)>abs(y)) sign(x)
  else sign(y)
}

#' Simulate one trajectory ofa Kendall random walk
#'
#' @param trajectory_length Number of samples to simulate.
#' @param step_dist Function that returns random numbers from step distribution.
#' @param alpha Alpha parameter of the random walk
#' @param ... Additional parameters to step distribution.
#'
#' @return Generated path of the random walk.
#'
#'

simulateOneTrajectory <- function(trajectory_length, step_dist,
                                  alpha, ...) {
  Y <- step_dist(trajectory_length, ...)
  theta <- EnvStats::rpareto(trajectory_length, 1, 2*alpha)*sample(c(-1, 1), trajectory_length, prob = c(0.5, 0.5), replace = TRUE)
  Xn <- vector("numeric", trajectory_length )
  Xn[1:2] <- c(0, Y[1])

  for(i in 2:(trajectory_length - 1)) {
    Xn[i+1] <- max(abs(Xn[i]), abs(Y[i+1]))*theta[i]^Qn(Xn[i],
                                                        Y[i+1],
                                                        alpha)*U(Xn[i], Y[i + 1])
  }
  Xn
}

#' Simulate multiple trajectories of Kendall random walk
#'
#' @param number_of_simulations number of trajectories to generate.
#' @param trajectory_length length of trajectories.
#' @param step_dist function returning random numbers from step dist.
#' @param alpha alpha parameter.
#' @param ... parameters for step distribution.
#'

simulation <- function(number_of_simulations, trajectory_length,
                       step_dist, alpha, ...) {
  listTmp <- as.list(1:number_of_simulations)
  tmp <- lapply(listTmp, function(l)
    simulateOneTrajectory(trajectory_length, step_dist,
                          alpha, ...))
  lapply(listTmp,
         function(x) tibble::tibble(simNo = x, sim = tmp[[x]])) %>%
    dplyr::bind_rows()
}


#' Normalising Kendall random walks
#'
#' @param simulations tibble returned by simulation function
#' @param an_seq sequence that the trajectories will be multiplied by
#' @param bn_seq sequence that will be substracted from scaled trajectory
#'
#' @return tibble
#'

normalising_sequences <- function(simulations, an_seq = 1, bn_seq = 0) {
  simulations %>%
    dplyr::mutate(simNo = as.factor(as.character(simNo))) %>%
    dplyr::group_by(simNo) %>%
    dplyr::mutate(sim = an_seq*sim - bn_seq)
}

#' Draw simulated trajectories
#'
#' @param simulations tibble returned by normalising_sequences function
#' @param max_x maximum value on x axis
#'
#' @return ggplot2 object
#'

visualize_convergence <- function(simulations, max_x = NULL) {
  nSim <- max(unique(as.integer(as.character(simulations$simNo))))
  trajectory_length <- dim(simulations)[1]/nSim
  if(is.null(max_x)) max_x <- trajectory_length

   simulations %>%
    dplyr::ungroup() %>%
    dplyr::mutate(x = rep(1:trajectory_length, nSim)) %>%
    dplyr::filter(x <= max_x) %>%
    ggplot2::ggplot(aes(x = x, y = sim, group = simNo)) +
      ggplot2::geom_line() +
      ggplot2::theme_bw() +
      ggplot2::xlab("") +
      ggplot2::ylab("") +
      ggplot2::guides(color = "none")
}

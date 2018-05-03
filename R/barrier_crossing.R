#' Estimate the distribution of first ladder moment for given level
#'
#' @param simulations kendall_simulation object
#' @param level Positive numeric
#'
#' @return tibble
#'
#' @export
#'

ladder_moment <- function(simulations, level) {
  if(level < 0) stop("Level must be positive")
  kendall_rw <- simulations$simulation
  kendall_rw <- dplyr::group_by(kendall_rw, sim_id)
  kendall_rw <- dplyr::mutate(kendall_rw, id = 1:n())
  kendall_rw <- dplyr::filter(kendall_rw, sim > level)
  kendall_rw <- dplyr::summarise(kendall_rw, ladder_moment = min(id))
  kendall_rw <- dplyr::ungroup(kendall_rw)
  class(kendall_rw) <- c("kendall_barier_crossing", class(kendall_rw))
  kendall_rw
}


#' Estimate the distribution of first ladder height for given level
#'
#' @param simulations kendall_simulation object
#' @param level Positive numeric
#'
#' @return tibble
#'
#' @export
#'

ladder_height <- function(simulations, level) {
  if(level < 0) stop("Level must be positive")
  kendall_rw <- simulations$simulation
  kendall_rw <- dplyr::group_by(kendall_rw, sim_id)
  kendall_rw <- dplyr::mutate(kendall_rw, id = 1:n())
  kendall_rw <- dplyr::filter(kendall_rw, sim > level)
  kendall_rw <- dplyr::summarise(kendall_rw, ladder_moment = min(sim))
  kendall_rw <- dplyr::ungroup(kendall_rw)
  class(kendall_rw) <- c("kendall_barier_crossing", class(kendall_rw))
  kendall_rw
}

#' Generic function for printing result of ladder_moment function
#'
#' @param x kendall_barrier_crossing object
#' @param ... Additional arguments
#'
#' @return invisible x
#'
#' @importFrom stats quantile sd
#'
#' @export
#'

print.kendall_barier_crossing <- function(x, ...) {
  quantiles <- quantile(x$ladder_moment, na.rm = T,
                        probs = seq(0, 1, by = 0.1))
  labels <- names(quantiles)
  cat("Mean of the distribution: ", mean(x$ladder_moment, na.rm = T), "\n")
  cat("Standard deviation of the distribution: ", sd(x$ladder_moment, na.rm = T), "\n")
  cat("Number of observations: ", max(x$sim_id), "\n")
  cat("Times the level was not crossed: ", sum(!is.finite(x$ladder_moment)), "\n")
  cat("Quantiles of the distribution: \n")
  # cat(labels, "\n")
  print(quantiles)
  invisible(x)
}


#' Generic function for plotting results of ladder_moment function.
#'
#' @param x kendall_barrier_crossing object
#' @param ... Additional arguments
#'
#' @return ggplot2
#'
#' @export
#'

plot.kendall_barier_crossing <- function(x, ...) {
  mean_value <- mean(x$ladder_moment)
  ggplot2::ggplot(x, ggplot2::aes(x = ladder_moment)) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(xintercept = mean_value) +
    ggplot2::theme_bw() +
    ggplot2::xlab("First ladder moments") +
    ggplot2::ylab("Count")
  # dodac tu ecdf
}

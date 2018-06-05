#' Estimate the distribution of first ladder moment for given level
#'
#' NA is returned if the level wasn't crossed. Printing the resulting object
#' will give summary of the estimated distribution and information whether
#' level wasn't crossed in some simulations. This information can be used to
#' pick the right trajectory length for the given level.
#'
#' @param simulations kendall_simulation object
#' @param level Positive numeric
#'
#' @return tibble
#'
#' @importFrom dplyr n
#'
#' @export
#'
#' @examples {
#' kendall_rw <- simulate_kendall_rw(100, 100, runif, 0.5)
#' estim_ladder <- ladder_moment(kendall_rw, 1000)
#' estim_ladder
#' }
#'

ladder_moment <- function(simulations, level) {
  if(level < 0) stop("Level must be positive")
  sim <- id <- sim_id <- NULL
  kendall_rw <- simulations$simulation
  kendall_rw <- dplyr::group_by(kendall_rw, sim_id)
  kendall_rw <- dplyr::mutate(kendall_rw, id = 1:n())
  all_sims <- dplyr::ungroup(dplyr::distinct(kendall_rw, sim_id))
  kendall_rw <- dplyr::filter(kendall_rw, sim > level)
  kendall_rw <- dplyr::summarise(kendall_rw, ladder_moment = min(id))
  kendall_rw <- dplyr::ungroup(kendall_rw)
  all_sims <- dplyr::left_join(all_sims, kendall_rw, by = "sim_id")

  class(all_sims) <- c("kendall_barrier_crossing", class(all_sims))
  all_sims
}


#' Estimate the distribution of first ladder height for given level
#'
#' NA is returned if the level wasn't crossed. Printing the resulting object
#' will give summary of the estimated distribution and information whether
#' level wasn't crossed in some simulations. This information can be used to
#' pick the right trajectory length for the given level.
#'
#' @param simulations kendall_simulation object
#' @param level Positive numeric
#'
#' @return tibble
#'
#' @importFrom dplyr n
#'
#' @export
#'
#' @examples {
#'   kendall_rw <- simulate_kendall_rw(100, 100, runif, 0.5)
#'   estim_ladder <- ladder_height(kendall_rw, 1000)
#'   estim_ladder
#' }
#'

ladder_height <- function(simulations, level) {
  if(level < 0) stop("Level must be positive")
  sim <- id <- sim_id <- NULL
  kendall_rw <- simulations$simulation
  kendall_rw <- dplyr::group_by(kendall_rw, sim_id)
  kendall_rw <- dplyr::mutate(kendall_rw, id = 1:n())
  all_sims <- dplyr::ungroup(dplyr::distinct(kendall_rw, sim_id))
  kendall_rw <- dplyr::filter(kendall_rw, sim > level)
  kendall_rw <- dplyr::summarise(kendall_rw, ladder_moment = min(sim))
  kendall_rw <- dplyr::ungroup(kendall_rw)
  all_sims <- dplyr::left_join(all_sims, kendall_rw, by = "sim_id")

  class(all_sims) <- c("kendall_barrier_crossing", class(all_sims))
  all_sims
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

print.kendall_barrier_crossing <- function(x, ...) {
  quantiles <- quantile(x$ladder_moment, na.rm = T,
                        probs = seq(0, 1, by = 0.1))
  labels <- names(quantiles)
  cat("Mean of the distribution: ", mean(x$ladder_moment, na.rm = T), "\n")
  cat("Standard deviation of the distribution: ", sd(x$ladder_moment, na.rm = T), "\n")
  cat("Number of observations: ", max(x$sim_id), "\n")
  cat("Times the level was not crossed: ", sum(!is.finite(x$ladder_moment)), "\n")
  cat("Quantiles of the distribution: \n")
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

plot.kendall_barrier_crossing <- function(x, ...) {
  mean_value <- mean(x$ladder_moment, na.rm = TRUE)
  ggplot2::ggplot(x, ggplot2::aes_string(x = 'ladder_moment')) +
    ggplot2::geom_histogram() +
    ggplot2::geom_vline(xintercept = mean_value) +
    ggplot2::theme_bw() +
    ggplot2::xlab("First ladder moments") +
    ggplot2::ylab("Count")
  # dodac tu ecdf
}

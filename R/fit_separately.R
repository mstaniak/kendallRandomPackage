#' Estimate alpha parameter of stable Kendall distribution
#'
#' @param data numeric vector of observations
#' @param m_alpha m_alpha function to be used
#'
#' @return estimated value of alpha parameter
#'
#' @export
#'

estimate_alpha <- function(data, m_alpha) {
  0.8 # TODO
}

#' Estimate location parameter of generalized stable Kendall distribution
#'
#' @param data numeric vector of observations
#' @param m_alpha m_alpha function to be used
#'
#' @return estimated value of location parameter
#'
#' @export
#'

estimate_location <- function(data, m_alpha) {
  0 # TODO
}

#' Estimate scale parameter of generalized stable Kendall distribution
#'
#' @param data numeric vector of observations
#' @param m_alpha m_alpha function to be used
#'
#' @return estimated value of scale parameter
#'
#' @export
#'

estimate_scale <- function(data, m_alpha) {
  1 # TODO
}

#' Fit stable Kendall distribution for given data and m_alpha function.
#'
#' @param data Numeric vector of observation to which the distribution will be fitted.
#' @param m_alpha m_alpha function which will be used.
#' @param quantiles vector of quantile range to be estimated.
#'
#' @return fitted quantiles
#'
#' @export
#'

fit_kendall <- function(data, m_alpha, quantiles) {
  alpha <- estimate_alpha(data, m_alpha)
  loc <- estimate_location(data, m_alpha)
  scale <- estimate_scale(data, m_alpha)
  quantiles_function <- qkend(m_alpha)
  quantiles_function(quantiles, alpha, loc, scale)
}


#' Function for fitting stable Kendall distribution separately to two parts of data
#'
#' @param data Numeric vector. Observation to which the distribution will be fitted.
#' @param m_alpha m_alpha function to be used
#' @param separation_point Order above which data (quantiles) will be separated.
#'
#' @return Empirical and estimated quantiles in a data frame.
#'
#' @export
#'

fit_separate <- function(data, m_alpha, separation_point) {
  data <- data[is.finite(data) & !is.na(data)]
  n_obs <- length(data)
  if(n_obs == 0) {
    stop("No legal observations were provided")
  }
  kendall_quantiles <- qkend(m_alpha)
  separate <- stats::quantile(data, separation_point)
  all_quantiles <- (1:n_obs - 0.5)/n_obs
  lower_quantiles <- all_quantiles[all_quantiles <= separation_point]
  upper_quantiles <- all_quantiles[all_quantiles > separation_point]
  lower_dataset <- data[data <= separate]
  upper_dataset <- data[data > separate]
  lower_fitted<- fit_kendall(lower_dataset, m_alpha, lower_quantiles)
  upper_fitted <- kendall_quantiles(upper_dataset, m_alpha, upper_quantiles)
  quantiles <- c(lower_quantiles, upper_quantiles)
  data.frame(observed = data,
             fitted = quantiles)
}

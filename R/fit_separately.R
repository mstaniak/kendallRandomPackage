#' Log-likelihood for stable kendall distribution with m_alpha = 1
#'
#' @param alpha alpha parameter of the Kendall random walk
#' @param x numeric vector of observations
#'
#' @return numeric
#'

kendall_loglik <- function(alpha, x) {
  x <- x[is.finite(x) & !is.na(x)]
  length(x)*log(alpha) - (2*alpha + 1)*sum(log(x)) - sum(x^(-alpha))
}


#' Optimize log-likelihood to find alpha parameter
#'
#' @param data numeric vector of observation
#'
#' @return numeric
#'
#' @export
#'

estimate_alpha <- function(data) {
  data <- data[is.finite(data) & !is.na(data)]
  optimize(f = kendall_loglik,  x = data, interval = c(0, 1), maximum = TRUE)$maximum
}  # For m_alpha = 1 - case of delta_1 step distribution



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
  alpha <- estimate_alpha(data)
  loc <- estimate_location(data)
  scale <- estimate_scale(data)
  quantiles_function <- qkend(m_alpha)
  fitted_quantiles <- quantiles_function(quantiles, alpha, loc, scale)
  list(fitted = fitted_quantiles,
       estimated_alpha = alpha,
       estimated_location = loc,
       estimated_scale = scale)
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
  all_lower_fit <- fit_kendall(lower_dataset, m_alpha, lower_quantiles)
  all_upper_fit <- fit_kendall(upper_dataset, m_alpha, upper_quantiles)
  lower_fitted<- unlist(all_lower_fit[[1]],
                        use.names = F)
  upper_fitted <- unlist(all_upper_fit[[1]],
                         use.names = F)
  quantiles <- unname(c(lower_fitted, upper_fitted))
  list(fit = data.frame(observed = sort(data),
                        fitted = quantiles),
       fitted_params = list(alpha_lower = all_lower_fit[[2]],
                            location_lower = all_lower_fit[[3]],
                            scale_lower = all_lower_fit[[4]],
                            alpha_upper = all_upper_fit[[2]],
                            location_upper = all_upper_fit[[3]],
                            scale_upper = all_upper_fit[[4]]))
}

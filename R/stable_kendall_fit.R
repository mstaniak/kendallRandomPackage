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

#' Fit alpha parameter using MLE for distribution with one parameter
#'
#' @param data Numeric vector
#'
#' @importFrom stats optimize
#'
#' @return list with optimal parameter and loglikelihood value
#'
#' @export
#'

estimate_stable_alpha <- function(data) {
  to_optimize <- function(x) kendall_loglik(x, data)
  optimize(to_optimize, c(0, 1), maximum = TRUE)
}


#' Fit stable Kendall distribution with one parameter (alpha)
#'
#' @param data Numeric vector of data.
#'
#' @return list of type kendall_fit
#'
#' @export
#'

fit_stable_alpha <- function(data) {
  alpha_param <- estimate_stable_alpha(data)$maximum
  qKend <- qkend(function(x) 1)
  theoretical_quantiles <- qKend((1:length(data) - 0.5)/length(data), alpha_param)
  fitted_list <- list(fit = tibble::tibble(observed_quantiles = sort(data),
                                           fitted_quantiles = sort(theoretical_quantiles)),
                      params = list(alpha = alpha_param))
  class(fitted_list) <- c("kendall_fit", "list")
  fitted_list
}

#' Negative loglikelihood for stable Kendall distr. with 3 parameters.
#'
#' @param data Dataset for which the loglikelihood will be calculated.
#'
#' @return numeric, value of loglikelihood
#'
#' @export
#'

full_minus_loglik <- function(data) {
  force(data)
  n <- length(data)
  function(parameters) {
    # data2 <- data
    alpha <- parameters[1]
    location <- parameters[2]
    scale <- parameters[3]
    scaled <- (data - location)/scale
    (2*alpha + 1)*sum(log(scaled)) + sum(scaled^(-alpha)) + n*log(scale) - n*log(alpha)
  }
}


#' Gradient of minus loglikelihood for stable Kendall distribution with 3 parameters.
#'
#' @param data numeric vector of observation.
#'
#' @return Function of one argument of length 3 (alpha, location, scale).
#'
#' @export
#'

full_loglik_gradient <- function(data) {
   function(parameters) {
     n <- length(data)
     (-1)*c(n/parameters[1] +
         sum(log((data - parameters[2])/parameters[3])*
               (((data - parameters[2])/parameters[3])^(-parameters[1]) - 2)),
       (2*parameters[1] + 1)*sum(1/(data - parameters[2])) -
         (parameters[3]^(parameters[1]))*
         parameters[1]*sum((data - parameters[2])^(-(parameters[1] + 1))),
       (2*n*parameters[1])/parameters[3] -
         parameters[1]*((parameters[3])^(parameters[1] - 1)) -
         sum((data - parameters[2])^(-parameters[1]))
     )
   }
}


#' Fit stable Kendall distribution for given data and m_alpha function.
#'
#' @param data Numeric vector of observation to which the distribution will be fitted.
#'
#' @importFrom stats optim
#'
#' @return fitted quantiles
#'
#' @export
#'

fit_kendall <- function(data) {
  alpha_start  <- estimate_stable_alpha(data)$maximum
  loglik_data <- full_minus_loglik(data)
  loglik_grad_data <- full_loglik_gradient(data)
  to_estimate <- optim(c(alpha_start, 0.1, 0.1), loglik_data, loglik_grad_data,
                 lower = c(0.01, -100, 0.001),
                 upper = c(0.99, min(data)-0.1, 100),
                 method = "L-BFGS-B")
  alpha <- to_estimate$par[1]
  loc <- to_estimate$par[2]
  scale <- to_estimate$par[3]
  quantiles_function <- qkend(function(x) 1)
  fitted_quantiles <- quantiles_function((1:length(data) - 0.5)/length(data),
                                         alpha, loc, scale)
  result <- list(fit = tibble::tibble(fitted_quantiles = fitted_quantiles,
                                      observed_quantiles = sort(data)),
                 params = list(estimated_alpha = alpha,
                               estimated_location = loc,
                               estimated_scale = scale))
  class(result) <- c("kendall_fit", "list")
  result
}


#' Function for fitting stable Kendall distribution separately to two parts of data
#'
#' @param data Numeric vector. Observation to which the distribution will be fitted.
#' @param separation_point Order above which data (quantiles) will be separated.
#'
#' @return List of class kendall_fit with estimated and theoretical quantiles
#' and estimated parameters.
#'
#' @export
#'

fit_separate <- function(data, separation_point) {
  data <- data[is.finite(data) & !is.na(data)]
  n_obs <- length(data)
  if(n_obs == 0) {
    stop("No legal observations were provided")
  }
  kendall_quantiles <- qkend(function(x) 1)
  separate <- stats::quantile(data, separation_point)
  all_quantiles <- (1:n_obs - 0.5)/n_obs
  lower_quantiles <- all_quantiles[all_quantiles <= separation_point]
  upper_quantiles <- all_quantiles[all_quantiles > separation_point]
  lower_dataset <- data[data <= separate]
  upper_dataset <- data[data > separate]
  all_lower_fit <- fit_kendall(lower_dataset)
  all_upper_fit <- fit_kendall(upper_dataset)
  lower_fitted<- unlist(all_lower_fit[[1]]$fitted_quantiles,
                        use.names = F)
  upper_fitted <- unlist(all_upper_fit[[1]]$fitted_quantiles,
                         use.names = F)
  quantiles <- unname(c(lower_fitted, upper_fitted))
  result <- list(fit = tibble::tibble(observed_quantiles = sort(data),
                                      fitted_quantiles = quantiles),
       params = list(alpha_lower = all_lower_fit[[2]]$estimated_alpha,
                     location_lower = all_lower_fit[[2]]$estimated_location,
                     scale_lower = all_lower_fit[[2]]$estimated_scale,
                     alpha_upper = all_upper_fit[[2]]$estimated_alpha,
                     location_upper = all_upper_fit[[2]]$estimated_location,
                     scale_upper = all_upper_fit[[2]]$estimated_scale))
  class(result) <- c("kendall_fit", "list")
  result
}


#' QQ-plot for the result of fitting stable Kendall distribtion.
#'
#' @param x List returned by fit_separate or fit_kendall function.
#' @param ... Aditional arguments.
#'
#' @return ggplot2 object
#'
#' @export
#'

plot.kendall_fit <- function(x, ...) {
  observed_vs_fitted <- x$fit
  ggplot2::ggplot(observed_vs_fitted, ggplot2::aes_string(x = 'observed_quantiles',
                                                          y = 'fitted_quantiles')) +
    ggplot2::theme_bw() +
    ggplot2::xlab("Empirical") +
    ggplot2::ylab("Theoretical quantile") +
    ggplot2::geom_point()
}


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


#' Optimize log-likelihood to find distribution parameters
#'
#' @param data numeric vector of observation.
#' @param initial_point Initial guess for true parameters.
#'
#' @return Function of one argument of length 3 (alpha, location, scale)
#'         which will be used to solve nonlinear system of equations
#'         (find zeros of the derivative of log-likelihood).
#'
#' @export
#'

estimate_parameters <- function(data, initial_point) {
   fun <- function(parameters) {
     n <- length(data)
     c(n/parameters[1] +
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
   nleqslv::nleqslv(initial_point, fun)$x
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
  n <- length(data)
  scaled <- (data - location)/scale
  function(parameters) {
    alpha <- parameters[1]
    location <- parameters[2]
    scale <- location[3]
    (2*alpha + 1)*sum(log(scaled)) + sum(scaled^(-alpha)) + n*log(scale) - n*log(alpha)
  }
}

#' Fit stable Kendall distribution for given data and m_alpha function.
#'
#' @param data Numeric vector of observation to which the distribution will be fitted.
#' @param m_alpha m_alpha function which will be used.
#' @param quantiles vector of quantile range to be estimated.
#' @param initial_point Initial guess for true parameters.
#'
#' @return fitted quantiles
#'
#' @export
#'

fit_kendall <- function(data, m_alpha, quantiles, initial_point) {
  to_estimate <- estimate_parameters(data, initial_point)
  alpha <- to_estimate[1]
  loc <- to_estimate[2]
  scale <- to_estimate_scale[3]
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
#' @param initial_points Initial guesses for true parameters.
#'
#' @return Empirical and estimated quantiles in a data frame.
#'
#' @export
#'

fit_separate <- function(data, m_alpha, separation_point, initial_points) {
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
  all_lower_fit <- fit_kendall(lower_dataset, m_alpha, lower_quantiles, initial_points[[1]])
  all_upper_fit <- fit_kendall(upper_dataset, m_alpha, upper_quantiles, initial_points[[2]])
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


#' Quick QQ-plot for the result of fitting two stable Kendall distributions.
#'
#' @param fit_list List returned by fit_separate function.
#'
#' @return ggplot2 object
#'
#' @export
#'

plot_qq <- function(fit_list) {
  observed_vs_fitted <- fit_list$fit
  ggplot(observed_vs_fitted, aes(x = observed, y = fitted)) +
    theme_bw() +
    xlab("Empirical") +
    ylab("Theoretical quantile") +
    geom_point()
}


#' Fit stable Kendall distribution to multiple sets of observations
#'
#' @param srcFrame tibble returned by calculateMaxima function
#' @param alpha value of alpha parameter
#' @param normalise If TRUE, maximums will be scaled and centered.
#' @param groupingVariables chr, vector of names of columns to group by,
#'        year and polutant by default
#' @param m_alpha function giving moment of order alpha of the step distribution
#' @param symmetric if TRUE, symmetrical version of stable Kendall distribution will be used
#'
#' @return tibble with empirical CDF, theoretical CDF and theoretical quantiles.
#'
#' @export
#'

addMultiKendall <- function(srcFrame, alpha, normalise = FALSE,
                            groupingVariables = c("year", "polutant"),
                            m_alpha = function(x) x, symmetric = FALSE) {
  if(symmetric) cdf <- pkendSym(m_alpha)
  else cdf <- pkend(m_alpha)
  tmp <- srcFrame %>%
    dplyr::filter(is.finite(maximum)) %>%
    dplyr::filter(length(unique(maximum)) > 2) %>%
    dplyr::group_by_(.dots = groupingVariables) %>%
    dplyr::arrange(maximum) %>%
    dplyr::mutate(alphar_parameter = alpha)
  if(normalise) {
    tmp %>%
      dplyr::mutate(maximum = as.numeric(scale(maximum))) %>%
      dplyr::mutate(empirical = (1:n())/n(),
                    theoretical = cdf(maximum, unique(alphar_parameter)))

  } else {
    tmp %>%
      dplyr::mutate(empirical = (1:n())/n(),
                    theoretical = cdf(maximum, unique(alphar_parameter)))

  }
}


#' Compare theorical and empirical CDF for stable Kendall distribution.
#'
#' @param sourceFrame tibble returned by addMultiKendall()
#'
#' @return ggplot2 object
#'

cdfsKendall <- function(sourceFrame) {
  sourceFrame %>%
    tidyr::gather(CDF, value, theoretical, empirical) %>%
    ggplot2::ggplot(aes(x = maximum, y = value, color = CDF)) +
    ggplot2::geom_point() +
    ggplot2::theme_bw() +
    ggplot2::ylab("")
}

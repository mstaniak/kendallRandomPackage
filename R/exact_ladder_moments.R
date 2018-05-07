#' Function G(t) - Williamson transform taken at point 1/t.
#'
#' This function return the whole "integrate" object, so precision of the approximation
#' can be checked.
#'
#' @param t Argument to the function.
#' @param alpha Value of the alpha parameter.
#' @param density Density function of the step distribution.
#'
#' @return Object of class "integrate"
#'
#' @importFrom stats integrate
#'
#' @examples
#' g_function_single(5, 0.26, dnorm)
#'
#' @export
#'

g_function_single <- function(t, alpha, density) {
  force(t)
  under_integral <- function(x) {
    (1 - abs(x/t)^(alpha))*density(x)
  }
  if(t != 0) {
    integrate(under_integral, lower = -abs(t), upper = abs(t))
  } else {
    fun <- function(x) x
    integrate(fun, lower = -1, upper = 1)
  }
}


#' Function G(t) - Williamson transform taken at point 1/t.
#'
#' This function return only approximated values. To check their precisions use
#' g_function_single function with an argument of length 1.
#'
#' @param t Argument to the function.
#' @param alpha Value of the alpha parameter.
#' @param density Density function of the step distribution.
#'
#' @return Object of class "integrate"
#'
#' @importFrom stats integrate
#'
#' @export
#'
#' @examples
#' g_function(1:5, 0.75, dnorm)
#'
#'

g_function <- function(t, alpha, density) {
  sapply(t, function(x) g_function_single(x, alpha, density)$value)
}


#' Distribution of the first ladder moment.
#'
#' @param n Argument to the PDF.
#' @param level Level a to be crossed.
#' @param alpha Alpha parameter of Kendall random walk.
#' @param step_cdf CDF of the step distribution.
#' @param step_pdf PDF of the step distribution.
#'
#' @return Value of PMF of the distribution of first ladder moment
#'
#' @export
#'
#' @examples
#' prob <- ladder_moment_pmf(10, 1000, 0.5, pnorm, dnorm)
#' prob
#'
#'

ladder_moment_pmf <- function(n, level, alpha, step_cdf, step_pdf) {
  Ga <- g_function(level, alpha, step_pdf)
  Fa <- step_cdf(level)
  Ha <- 2*Fa - 1 - Ga
  A <- 1 + Ha/((2*Ga - 1)^2) - Ga/(2*Ga - 1)
  B <- Ha/((1 - Ga)*(2*Ga - 1))
  C <- Ga/(2*Ga - 1) - (Ha*Ga)/(((2*Ga - 1)^2)*(1 - Ga))

  A*(2^(-n)) + B*n*((1 - Ga)^2)*(Ga^(n - 1)) + C*(1 - Ga)*(Ga^(n - 1))
}

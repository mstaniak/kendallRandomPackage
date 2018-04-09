#' PDF of Kendall stable distribution
#'
#' @param m_alpha function giving moments of order alpha of step dist.
#'
#' @return function that returns values of the PDF
#'
#' @export
#'
#' @examples {
#' dKend <- dkend(function(x) 1)
#' # Step distribution: delta_{1}
#' dKendall <- dKend(1:10, 0.5)
#' # Values of PDF for arguments 1:10 and alpha = 0.5
#' }
#'

dkend <- function(m_alpha) {
  force(m_alpha)
  function(x, alpha, mu = 0, sigma = 1) {
    sapply(x, function(y) {
      if(!is.finite(y)) return(NA)
      else if((y - mu)/sigma <= 0) 0
      else (alpha/sigma)*(m_alpha(alpha)^2)*((y - mu)/sigma)^(-(2*alpha + 1))*exp(-m_alpha(alpha)*((y - mu)/sigma)^(-alpha))
    })
  }
}


#' CDF of Kendall stable distribution
#'
#' @param m_alpha function giving moments of order alpha of step dist.
#'
#' @return function function giving values of CDF of Kendall stable distribution
#'
#' @export
#'
#' @examples {
#' pKend <- pkend(function(x) 1)
#' # Step distribution: delta_{1}
#' pKendall <- pKend(1:10, 0.5)
#' # Values of CDF for arguments 1:10 and alpha = 0.5
#' }
#'

pkend <- function(m_alpha) {
  force(m_alpha)
  function(x, alpha, mu = 0, sigma = 1) {
    sapply(x, function(y) {
      if(!is.finite(y)) return(NA)
      else if((y - mu)/sigma <= 0) 0
      else (1 + m_alpha(alpha)*((y - mu)/sigma)^(-alpha))*exp(-m_alpha(alpha)*((y - mu)/sigma)^(-alpha))
    })
  }
}


#' Quantiles of Kendall stable distribution
#'
#' @param m_alpha function giving moments of order alpha of step dist.
#'
#' @return function function returning quantiles of given orders
#'
#' @export
#'
#' @examples {
#' qKend <- qkend(function(x) 1)
#' # Step distribution: delta_{1}
#' qKendall <- qKend(c(0.1, 0.9), 0.5)
#' # Quantiles of order 0.1 and 0.9 for alpha = 0.5
#' }
#'

qkend<- function(m_alpha) {
  force(m_alpha)
  function(p, alpha, mu = 0, sigma = 1) {
    oCDF <- function(x) pkend(m_alpha)(x, alpha, mu, sigma)
    sapply(p, function(q) {
      if(!is.finite(q)) return(NA)
      else stats::uniroot({function(x) oCDF(x) - q}, lower = 0, upper = 10^80)$root
    })
  }
}


#' Pseudo-random number from Kendall stable distribution
#'
#' @param m_alpha function giving moments of order alpha of step dist.
#'
#' @return function return n numbers genereted from Kendall stable dist.
#'
#' @export
#'
#' @examples {
#' rKend <- rkend(function(x) 1)
#' # Step distribution: delta_{1}
#' rKendall <- rKend(10, 0.5)
#' # Ten random number from stable Kendall distribution with alpha = 0.5
#' }
#'


rkend <- function(m_alpha) {
  force(m_alpha)
  qKend <- qkend(m_alpha)
  function(n, alpha, mu = 0, sigma = 1) {
    sapply(1:n, {function(x)
      qKend(stats::runif(1, 0, 1), alpha, mu, sigma)
    })
  }
}


#' CDF of symmetrical Kendall stable distribution
#'
#' @param m_alpha function giving moments of order alpha of step dist.
#'
#' @return function function giving values of CDF of Kendall stable distribution
#'
#' @export
#'
#' @examples {
#' pKend <- pkendSym(function(x) 1)
#' # Step distribution: delta_{1}
#' pKendall <- pKend(1:10, 0.5)
#' # Values of CDF for arguments 1:10 and alpha = 0.5
#' }
#'

pkendSym <- function(m_alpha) {
  force(m_alpha)
  function(x, alpha, mu = 0, sigma = 1) {
    Ft <- function(y) {
      0.5*(1 + m_alpha(alpha)*(y^((-1)*alpha)) + exp(m_alpha(alpha)*(y^((-1)*alpha))))*exp((-1)*m_alpha(alpha)*(y^((-1)*alpha)))
    }
    sapply(x, function(y) {
      if(!is.finite(y)) return(NA)
      else if(y == 0) 0.5
      else if(y > 0) Ft(y)
      else 1 - Ft((-1)*y)
    })
  }
}


#' Quantiles of symmetrical Kendall stable distribution
#'
#' @param m_alpha function giving moments of order alpha of step dist.
#'
#' @return function function returning quantiles of given orders
#'
#' @export
#'
#' @examples {
#' qKend <- qkendSym(function(x) 1)
#' # Step distribution: delta_{1}
#' qKendall <- qKend(c(0.1, 0.9), 0.5)
#' # Quantiles of order 0.1 and 0.9 for alpha = 0.5
#' }
#'

qkendSym <- function(m_alpha) {
  force(m_alpha)
  function(p, alpha, mu = 0, sigma = 1) {
    oCDF <- function(x) pkendSym(m_alpha)(x, alpha)
    sapply(p, function(q) {
      if(!is.finite(q)) return(NA)
      else if(q >= 0.5) stats::uniroot({function(x) oCDF(x) - q}, lower = 0, upper = 10^80)$root
      else (-1)*stats::uniroot({function(x) oCDF(x) - (1 - q)}, lower = 0, upper = 10^80)$root
    })
  }
}

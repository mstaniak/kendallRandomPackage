#' PDF of Kendall stable distribution
#' 
#' @param mAlpha function giving moments of order alpha of step dist.
#'
#' @return function that returns values of the PDF
#' 
#' @export
#' 
#' @examples 
#' dKend <- dkend(function(x) 1)
#' # Step distribution: delta_{1}
#' dKendall <- dKend(1:10, 0.5)
#' # Values of PDF for arguments 1:10 and alpha = 0.5
#' 

dkend <- function(mAlpha) {
  force(mAlpha)
  function(x, alpha) {
    sapply(x, function(y) {
      if(!is.finite(y)) return(NA)
      else if(y <= 0) 0
      else alpha*(mAlpha(alpha)^2)*y^(-(2*alpha + 1))*exp(-mAlpha(alpha)*y^(-alpha))
    })
  }
}


#' CDF of Kendall stable distribution
#' 
#' @param mAlpha function giving moments of order alpha of step dist.
#' 
#' @return function function giving values of CDF of Kendall stable distribution
#' 
#' @export
#' 
#' @examples 
#' pKend <- pkend(function(x) 1)
#' # Step distribution: delta_{1}
#' pKendall <- dKend(1:10, 0.5)
#' # Values of CDF for arguments 1:10 and alpha = 0.5
#' 

pkend <- function(mAlpha) {
  force(mAlpha)
  function(x, alpha) {
    sapply(x, function(y) {
      if(!is.finite(y)) return(NA)
      else if(y <= 0) 0
      else (1 + mAlpha(alpha)*y^(-alpha))*exp(-mAlpha(alpha)*y^(-alpha))
    })
  }
}


#' Quantiles of Kendall stable distribution
#' 
#' @param mAlpha function giving moments of order alpha of step dist.
#' 
#' @return function function returning quantiles of given orders
#' 
#' @export
#' 
#' @examples 
#' qKend <- qkend(function(x) 1)
#' # Step distribution: delta_{1}
#' qKendall <- qKend(c(0.1, 0.9), 0.5)
#' # Quantiles of order 0.1 and 0.9 for alpha = 0.5
#' 

qkend<- function(mAlpha) {
  force(mAlpha)
  function(p, alpha) {
    oCDF <- function(x) pkend(mAlpha)(x, alpha)
    sapply(p, function(q) {
      if(!is.finite(q)) return(NA)
      else uniroot({function(x) oCDF(x) - q}, lower = 0, upper = 10^80)$root
    })
  }
}


#' Pseudo-random number from Kendall stable distribution
#' 
#' @param mAlpha function giving moments of order alpha of step dist.
#' 
#' @return function return n numbers genereted from Kendall stable dist.
#' 
#' @export
#' 
#' @examples 
#' rKend <- rkend(function(x) 1)
#' # Step distribution: delta_{1}
#' rKendall <- rKend(10, 0.5)
#' # 10 pseudo-random numbers for alpha = 0.5
#' 

rkend <- function(mAlpha) {
  force(mAlpha)
  function(n, alpha) {
    sapply(1:n, {function(x)
      qkend(runif(1, 0, 1), mAlpha, alpha)   
    })
  }
}

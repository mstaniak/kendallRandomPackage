#' Fit GEV distribution to multiple sets of observations
#'
#' @param srcFrame tibble returned by calculateMaxima function
#' @param groupingVariables chr, vector of names of columns to group by,
#'        year and polutant by default
#' @param full lgl, if TRUE, tibble from srcFrame argument will be returned
#'        with additional columns giving parameter values of fitted GEV dist.
#'        If FALSE, just parameters (with corresponding values of grouping variables)
#'        will be returned.
#' @param ... additional parameters passed to egevd() function
#'
#' @return tibble as described in help for full argument
#'
#' @export
#'

fitMultiGEV <- function(srcFrame, groupingVariables = c("year", "polutant"), full = TRUE, ...) {
  tmp <- srcFrame %>%
    dplyr::filter(is.finite(maximum)) %>%
    dplyr::group_by_(.dots = groupingVariables) %>%
    dplyr::filter(length(unique(maximum)) > 2) %>%
    dplyr::summarise(location = EnvStats::egevd(maximum, ...)$parameters[1],
                     scale = EnvStats::egevd(maximum, ...)$parameters[2],
                     shape = EnvStats::egevd(maximum, ...)$parameters[3])
  if(full) srcFrame %>%
    dplyr::left_join(tmp, by = groupingVariables) %>%
    dplyr::arrange_(.dots = groupingVariables)
  else tmp
}


#' QQ-plot for fited GEV distribution
#'
#' @param sourceFrame tibble returned by filteredData()
#' @param fittedGEV list returned by egevd function
#'
#' @return ggplot2 object
#'

qqPlotGev <- function(sourceFrame, fittedGEV) {
  sourceFrame %>%
    dplyr::mutate(par1 = fittedGEV$parameters[1],
	         par2 = fittedGEV$parameters[2],
	         par3 = fittedGEV$parameters[3]) %>%
    dplyr::filter(is.finite(maximum)) %>%
    dplyr::arrange(maximum) %>%
    dplyr::mutate(no = 1:nrow(.)) %>%
    dplyr::mutate(theoretical = qgevd(no/(max(no) + 1), par1, par2, par3),
	                empirical = stats::quantile(maximum, no/(max(no) + 1))) %>%
    ggplot2::ggplot(aes(x = theoretical, y = empirical)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw()
}

#' Compare theorical and empirical CDF for fitted GEV distribution.
#'
#' @param sourceFrame tibble returned by filteredData()
#' @param fittedGEV list returned by egevd function
#'
#' @return ggplot2 object
#'

cdfsGev <- function(sourceFrame, fittedGEV) {
  sourceFrame %>%
    dplyr::mutate(par1 = fittedGEV$parameters[1],
           par2 = fittedGEV$parameters[2],
           par3 = fittedGEV$parameters[3]) %>%
    dplyr::filter(is.finite(maximum)) %>%
    dplyr::arrange(maximum) %>%
    dplyr::mutate(no = 1:nrow(.)) %>%
    dplyr::mutate(theoretical = pgevd(maximum, par1, par2, par3),
           empirical = no/max(no)) %>%
    tidyr::gather(CDF, value, theoretical, empirical) %>%
    ggplot2::ggplot(aes(x = maximum, y = value, color = CDF)) +
      ggplot2::geom_point() +
      ggplot2::theme_bw() +
      ggplot2::ylab("")
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

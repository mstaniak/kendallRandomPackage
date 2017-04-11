#' QQ-plot for fited GEV distribution
#' 
#' @param sourceFrame tibble returned by filteredData()
#' @param fittedGEV list returned by egevd function
#'
#' @return ggplot2 object
#'

qqPlotGev <- function(sourceFrame, fittedGEV) {
  sourceFrame %>%
    mutate(par1 = fittedGEV$parameters[1],
	   par2 = fittedGEV$parameters[2],
	   par3 = fittedGEV$parameters[3]) %>%
    filter(is.finite(maximum)) %>%
    arrange(maximum) %>%
    mutate(no = 1:nrow(.)) %>%
    mutate(theoretical = qgevd(no/(max(no) + 1), par1, par2, par3),
	   empirical = quantile(maximum, no/(max(no) + 1))) %>%
    ggplot(aes(x = theoretical, y = empirical)) +
      geom_point() +
      theme_bw()
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
    mutate(par1 = fittedGEV$parameters[1],
           par2 = fittedGEV$parameters[2],
           par3 = fittedGEV$parameters[3]) %>%
    filter(is.finite(maximum)) %>%
    arrange(maximum) %>%
    mutate(no = 1:nrow(.)) %>%
    mutate(theoretical = pgevd(maximum, par1, par2, par3),
           empirical = no/max(no)) %>%
    gather(CDF, value, theoretical, empirical) %>% 
    ggplot(aes(x = maximum, y = value, color = CDF)) +
    geom_point() +
    theme_bw() +
    ylab("")
}
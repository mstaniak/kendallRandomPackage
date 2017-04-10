#' QQ-plot for fited GEV distribution
#' 
#' @param sourceFrame tibble returned by filteredData()
#' @param fittedGEV list returned by egevd function
#'
#' @return ggplot2 object
#'
#' @export
#'

qqPlotGev <- function(sourceFrame, fittedGEV) {
  sourceFrame %>%
    mutate(par1 = fittedGEV$parameters[1],
	   par2 = fittedGEV$parameters[2],
	   par3 = fittedGEV$parameters[3]) %>%
    filter(is.finite(maximum)) %>%
    arrange(maximum) %>%
    mutate(no = 1:nrow(.)) %>%
    mutate(theoretical = qgevd(par1, par2, par3, no/(max(no) + 1)),
	   empirical = quantile(maximum, no/(max(no) + 1))) %>%
    ggplot2(aes(x = theoretical, y = empirical)) +
      geom_point() +
      theme_bw()
}

#' Plot histogram of values over given threshold
#' 
#' @param srcTbl tibble returned by filteredData() interactive
#' @param threshold cut-off value
#'
#' @return ggplot2 object
#'  

plotHist <- function(srcTbl, threshold) {
  src <- srcTbl %>%
    dplyr::filter(maximum > threshold) %>%
    dplyr::mutate(maximum = maximum - threshold)
  binW <- IQR(src$maximum)/(length(src$maximum)^(1/3))
  ggplot(src, aes(x = maximum)) +
    geom_histogram(binwidth = binW) +
    theme_bw()
}


#' QQ-plot for large quantiles
#' 
#' @param srcTbl tibble returned by filteredData()
#' @param alpha Kendall stable distribution parameter
#' @param minMaxQ minimum and maximum quantile to be used
#' @param stepQ step between minimum and maximum quantile
#' @param meanFunction function giving moment of order alpha of the step distribution
#' 
#' @return ggplot2 object
#' 

plotLargeQQ <- function(srcTbl, alpha, minMaxQ, stepQ, meanFunction = function(x) 1) {
  qSeq <- seq(minMaxQ[1], minMaxQ[2], stepQ)
  x <- srcTbl %>%
    dplyr::mutate(maximum = as.vector(scale(maximum))) %>%
    dplyr::filter(is.finite(maximum)) %>%
    dplyr::ungroup() %>%
    dplyr::select(maximum) %>%
    unlist(use.names = FALSE) %>%
    quantile(probs = qSeq, na.rm = TRUE)
  qLim <- qkend(meanFunction)
  y <- qLim(qSeq, alpha)
  tibble(x = x, y = y) %>%
    dplyr::filter(is.finite(x),
           is.finite(y),
           x < 10,
           y < 10) %>%
    ggplot(aes(x, y, label = round(y, 2))) +
      geom_point() +
      geom_smooth(method = "lm", se = FALSE) +
      geom_text() +
      theme_bw()
}


#' Standard QQ-plot
#' 
#' @param srcTbl tibble returned by filteredData()
#' @param alpha Kendall stable dist. parameter
#' @param meanFunction function giving moment of order alpha of the step distribution
#' @param threshold cut-off value for observations
#' 
#' @return ggplot2 object
#' 

plotQQ <- function(srcTbl, alpha, meanFunction = function(x) 1, threshold = 0) {
  x <- srcTbl %>%
    dplyr::filter(is.finite(maximum),
           maximum > threshold) %>%
    dplyr::mutate(maximum = maximum - threshold) %>%
    dplyr::mutate(maximum = as.vector(scale(maximum))) %>%
    dplyr::ungroup() %>%
    dplyr::select(maximum) %>%
    filter(is.finite(maximum)) %>%
    unlist(use.names = FALSE) %>%
    quantile(probs = (1:length(.) - 0.5)/(length(.))) # Do poprawy
  qLim <- qkend(meanFunction)
  y <- qLim((1:length(x) - 1)/(length(x) - 1), alpha)
  tibble(x = x, y = y) %>%
    ggplot(aes(x, y)) +
    geom_point() +
    geom_smooth(method = "lm", se = FALSE) +
    theme_bw()
}


#' Plot of data over time
#' 
#' @param srcTbl tibble returned by filteredData()
#' 
#' @return ggplot2 object
#' 

plotTime <- function(srcTbl, datesRange = "") {
  srcTbl %>%
    #     filter(dzienPomiaru >= datesRange[1],
    # 	   dzienPomiaru <= datesRange[2]) %>%
    ggplot(aes(x = measTime, y = maximum)) +
      geom_line() +
      theme_bw() +
      xlab("date") +
      ylab("measured value")
}

#' ECDF for data
#' 
#' @param srcTbl tibble returned by filteredData()
#' 
#' @return ggplot2 object
#' 

plotEcdf <- function(srcTbl) {
  ggplot(srcTbl, aes(x = maximum)) +
    stat_ecdf() +
    theme_bw()
}

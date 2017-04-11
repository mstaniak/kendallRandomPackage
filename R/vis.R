#' Plot histogram of values over given threshold
#' 
#' @param srcTbl tibble returned by filteredData() interactive
#' @param threshold cut-off value
#'
#' @return ggplot2 object
#'  

plotHist <- function(srcTbl, threshold) {
  src <- srcTbl %>%
    filter(maximum > threshold) %>%
    mutate(maximum = maximum - threshold)
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
#' 
#' @return ggplot2 object
#' 

plotLargeQQ <- function(srcTbl, alpha, minMaxQ, stepQ) {
  qSeq <- seq(minMaxQ[1], minMaxQ[2], stepQ)
  x <- srcTbl %>%
    mutate(maximum = as.vector(scale(maximum))) %>%
    filter(is.finite(maximum)) %>%
    ungroup() %>%
    select(maximum) %>%
    unlist(use.names = FALSE) %>%
    quantile(probs = qSeq)
  qLim <- qkend(function(x) x)
  y <- qLim(qSeq, alpha)
  tibble(x = x, y = y) %>%
    filter(is.finite(x),
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
#' @param threshold cut-off value for observations
#' 
#' @return ggplot2 object
#' 

plotQQ <- function(srcTbl, alpha, threshold = 0) {
  x <- srcTbl %>%
    filter(is.finite(maximum),
           maximum > threshold) %>%
    mutate(maximum = maximum - threshold) %>%
    mutate(maximum = as.vector(scale(maximum))) %>%
    ungroup() %>%
    select(maximum) %>%
    unlist(use.names = FALSE) %>%
    quantile(probs = seq(0.1, 0.9, 0.1)) # Do poprawy
  qLim <- qkend(function(x) x)
  y <- qLim(seq(0.1, 0.9, 0.1), alpha)
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

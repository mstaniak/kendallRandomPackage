plotHist <- function(srcTbl, threshold) {
  src <- srcTbl %>%
   filter(wartosc > threshold)
  binW <- IQR(src$wartosc)/(length(src$wartosc)^(1/3))
   ggplot(src, aes(x = wartosc)) +
     geom_histogram(binwidth = binW) +
     theme_bw()
}

plotLargeQQ <- function(srcTbl, quantiles = seq(0.8, 0.99, 0.05), alpha, minMaxQ, stepQ) {
  qSeq <- seq(minMaxQ[1], minMaxQ[2], stepQ)
  x <- srcTbl %>%
    mutate(wartosc = as.vector(scale(wartosc))) %>%
    select(wartosc) %>%
    unlist(use.names = FALSE) %>%
    quantile(probs = qSeq)
  qGran <- qgraniczny(function(x) x)
  y <- qGran(qSeq, alpha)
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

plotTime <- function(srcTbl, datesRange = "") {
  srcTbl %>%
#     filter(dzienPomiaru >= datesRange[1],
# 	   dzienPomiaru <= datesRange[2]) %>%
    ggplot(aes(x = dzien, y = wartosc)) +
      geom_line() +
      theme_bw() +
      xlab("date") +
      ylab("measured value")
}

plotEcdf <- function(srcTbl) {
  ggplot(srcTbl, aes(x = wartosc)) +
    stat_ecdf() +
    theme_bw()
}

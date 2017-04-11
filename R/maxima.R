#' Calculate maxima for a given station and time step.
#'
#' @param sourceFrame tibble returned by importGiosFromXLSX function.
#' @param block time step for maxima calculations.
#'
#' @return tibble with maxima grouped by year, date, polutant.
#'
#' @export
#'
#' @examples
#' obs <- importGiosFromXLSX("DsWrocKorzA", c("NOx", "SO2"), c("2015", "2014"))
#' obs2 <- calculateMaxima(obs)
#'

calculateMaxima <- function(sourceFrame, block = "day") {
  years <- unique(year(sourceFrame$measDate))
  sourceFrame %>%
    dplyr::mutate(measTime = round_date(measDate, block),
	   year = as.character(year(measDate))) %>%
    dplyr::group_by(year, polutant, measTime) %>%
    dplyr::summarise(maximum = max(measurement, na.rm = TRUE)) %>%
    dplyr::filter(year %in% years)
}

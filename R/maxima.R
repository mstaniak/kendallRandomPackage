#' Calculate maxima for a given station and time step.
#'
#' @param sourceFrame tibble returned by importGiosFromXLSX function.
#' @param block time step for maxima calculations.
#'
#' @return tibble with maxima grouped by year, date, polutant.
#'
#' @export
#'

calculateMaxima <- function(sourceFrame, block = "day") {
  sourceFrame %>%
    dplyr::mutate(measTime = lubridate::floor_date(measDate, block),
	   year = as.character(lubridate::year(measDate))) %>%
    dplyr::group_by(year, polutant, measTime) %>%
    dplyr::summarise(maximum = max(measurement, na.rm = TRUE))
}

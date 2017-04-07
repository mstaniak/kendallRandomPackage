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
  years <- unique(sourceFrame$year)
  sourceFrame %>%
    mutate(measTime = round_date(measDate, block))
    group_by(year, polutant, measTime) %>%
    summarise(maximum = max(measurement, na.rm = TRUE)) %>%
    filter(year %in% years)
}

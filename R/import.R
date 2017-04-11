#' Import one file from GIOŚ .xlsx file.
#'
#' @param station chr, name of the chosen station.
#' @param polutant chr, name of chosen polutant.
#' @param year chr, chosen year (yyyy).
#' @param path chr, path to folder containing .xlsx file or path to .xlsx file.
#' @param noHours chr, 1 or 24, defaults to 1 (hourly meas.)
#' @param skip int, number of rows to skip, defaults to 3.
#'        Do not change if unchanged GIOŚ files are used.
#' @param exact lgl, TRUE if path is a path to .xlsx file.
#'
#' @return tibble
#'

importOneXLSX <- function(station, polutant, year, path = getwd(),
                          noHours = "1", skip = 3, exact = FALSE) {
  
  fileList <- character(0)
  if(exact) fileList <- path
  else {
    fileList <- list.files(path, pattern = "*.xlsx") %>%
      grep(pattern = year, value = TRUE) %>%
      grep(pattern = polutant, value = TRUE) %>%
      grep(pattern = paste0(noHours, "g"), value = TRUE)
  }

  emptyFrame <- tibble(measDate = character(0),
		       station = character(0),
		       polutant = character(0),
		       measurement = character(0))

  if(length(fileList) == 0) return(emptyFrame)
  
  srcFile <- paste(path, fileList, sep = "/")
  colNames <- colnames(read_excel(srcFile))

  if(!sum(grepl(colNames, pattern = station))) {
    isOld  <- sum(grepl(names(stationCodes), pattern = station))
    isNew <- sum(grepl(stationCodes, pattern = station))
    if(isOld) {
      station <- stationCodes[station]
    } else if(isNew) {
      station <- names(stationCodes)[grep(stationCodes, pattern = station)]
    }
    if(!sum(grepl(colNames, pattern = station))) {
      return(emptyFrame)
    }
  }

  tmpFrame <- as_tibble(read_excel(srcFile, skip = skip, col_names = FALSE))
  colnames(tmpFrame) <- colNames
  colnames(tmpFrame)[1] <- "measDate"
  tmpFrame <- tmpFrame[, c("measDate", station)]
  colnames(tmpFrame)[2] <- "measurement"

  tmpFrame %>%
    dplyr::mutate(measurement = str_replace_all(measurement, ",", ".")) %>%
    dplyr::mutate(measurement = as.numeric(measurement),
	                station = station,
	                polutant = polutant)

}


#' Import data for one station, multiple years and polutants.
#'
#' @param station chr, name of the chosen station.
#' @param polutants chr, names of chosen polutants.
#' @param years chr, chosen years (yyyy).
#' @param path chr, path to folder containing .xlsx file or path to .xlsx file.
#' @param noHours chr, 1 or 24, defaults to 1 (hourly meas.)
#' @param skip int, number of rows to skip, defaults to 3.
#'        Do not change if unchanged GIOŚ files are used.
#' @param exact lgl, TRUE if path is a path to .xlsx file.
#'
#' @return tibble
#'
#' @export
#' 
#' @examples
#' importGiosFromXLSX("DsWrocKorzA", c("NOx", "SO2"), c("2015", "2014"))
#' # Default settings, .xlsx files are in the working directory.
#' importGiosFromXLSX("DsWrocKorzA", c("NOx", "SO2"), c("2015", "2014"), "path-to-the-folder")
#' # Importing from a different (not working) directory.
#'

importGiosFromXLSX <- function(station, polutants = NULL, years = NULL, path = getwd(), 
                               noHours = "1", skip = 3, exact = FALSE) {
  if(!exact & (is.null(polutants) | is.null(years))) stop("Years and polutants must be given if exact = FALSE")
  if(exact & path == getwd()) stop("Paths to files must be given if exact = TRUE")

  tmpResult <- vector("list", length(polutants)*length(years))
  for(i in polutants) {
    for(j in years) {
      tmpResult[[paste0(i, j)]] <- importOneXLSX(station, i, j, noHours, path, skip, exact)
    }
  }
  tmpResult %>%
    bind_rows() %>%
    dplyr::select(station, polutant, measDate, measurement) %>%
    dplyr::mutate(measDate = ymd_hms(measDate)) %>%
    dplyr::mutate(measDate = round_date(measDate, unit = "hour")) %>%
    dplyr::filter(year(measDate) %in% years)

}


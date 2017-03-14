

importOneXLSX <- function(station, polutant, year, noHours, path = getwd(), skip, exact = FALSE) {
  
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
  tmpFrame <- ramka[, c("measDate", stacja)]
  colnames(tmpFrame)[2] <- "measurement"

  tmpFrame %>%
    mutate(measurement = str_replace_all(measurement, ",", ".")) %>%
    mutate(measurement = as.numeric(measurement),
	   station = station,
	   polutant = polutant)

}



#'
#'
#'
#'
#'
#' @import
#'


importGiosFromXLSX <- function(station, polutants = NULL, years = NULL, noHours = 1, 
			       path = getwd(), skip = 3, exact = FALSE) {
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
    select(station, polutants, measDate, measurement) %>%
    mutate(measDate = ymd_hms(measDate)) %>%
    mutate(measDate = round_date(measDate, unit = "hour")) %>%
    filter(year(measDate) %in% years)

}


importGiosFromCSV <- function() {

}

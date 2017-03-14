

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
}


importGiosFromCSV <- function() {

}

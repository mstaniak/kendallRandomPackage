#' kendallRandomPackage: package for exploring air pollution data in Poland.
#'
#' Package provides functions that help import datasets from .xlsx files provided on GIOŚ website, 
#' convert them to the form suitable for analyses, calculate maxima over given period,
#' and visualize them using attached shiny app.
#' 
#' @section Important function:
#' \code{\link{importGiosFromXLSX}} imports data from multiple .xlsx files.
#' \code{\link}{importGiosFromCSV} does the same for .csv files converted from .xlsx files, which is faster.
#' \code{\link{calculateMaxima}} calculates maxima over given period for data important using \code{\link{importGiosFromXLSX}}.
#' \code{\link{kendallRandomApp}} starts a Shiny application that features multiple tools for exploring obtained data, 
#' including peak over threshold methods.
#'
#' @docType package
#' @name kendallRandomPackage
NULL
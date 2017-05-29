#' kendallRandomPackage: package for exploring air pollution data in Poland.
#'
#' Package provides functions that help import datasets from .xlsx files provided on GIOŚ website, 
#' convert them to the form suitable for analyses, calculate maxima over given period,
#' and visualize them using attached shiny app.
#' 
#' @section Important functions:
#' \code{\link{isAvailable}} checks if data are available in GIOŚ files.
#' \code{\link{importGiosFromXLSX}} imports data from multiple .xlsx files.
#' \code{\link{importGiosFromCSV}} does the same for .csv files converted from .xlsx files, which is faster.
#' \code{\link{calculateMaxima}} calculates maxima over given period for data important using \code{\link{importGiosFromXLSX}}.
#' \code{\link{fitMultiGEV}} fits GEV distribution using egevd() function from EnvStats package. 
#' Resulting table is suitable for further analysis or exploration (like plotting qq-plots, empirical and theoretical CDFs, etc).
#' \code{\link{addMultiKendall}} add empirical and theoretical CDF values and theoretical quantiles of stable kendall distribution
#' for given step distribution moment and alpha value.
#' \code{\link{kendallRandomApp}} starts a Shiny application that features multiple tools for exploring obtained data, 
#' including peak over threshold methods.
#'
#' @docType package
#' @name kendallRandomPackage
NULL
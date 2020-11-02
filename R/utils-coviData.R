#' Wrappers Around coviData Function
#'
#' \code{coviData} is the package for down/loading, munging, and performing
#' basic analysis on COVID-19 data for Shelby County. It includes functionality
#' for downloading data from the integrated data tool, which is re-exported in
#' covidCluster.
#'
#' @inheritParams coviData::download_integrated_data
#'
#' @export
download_integrated_data <- coviData::download_integrated_data

#' Wrappers Around coviData Function
#'
#' \code{coviData} is the package for down/loading, munging, and performing
#' basic analysis on COVID-19 data for Shelby County. It includes functionality
#' for loading data from the integrated data tool, which is re-exported in
#' covidCluster.
#'
#' @inheritParams coviData::load_integrated_data
#'
#' @export
load_integrated_data <- coviData::load_integrated_data

#' climateR
#'
#' \code{climateR} package
#'
#' Download gridded and point climate data
#'
#' See the README on
#'
#' @docType package
#' @name climateR
#'
#' @import AOI
#' @importFrom raster raster extent crop stack brick crs
#' @importFrom RNetCDF open.nc var.get.nc
#' @importFrom foreach %do% %dopar% foreach
#' @importFrom doParallel registerDoParallel
#' @importFrom httr write_disk GET

NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("model_meta", "grid_meta", "i"))

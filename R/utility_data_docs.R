#' Grids
#'
#' Lat/Long vectors for all data set options. Used for constructing subsetting calls to OPeNDAP.
#'
#' @docType data
#'
#' @format a \code{list} of lat/long vectors:
#' \itemize{
#' \item 'daymet':    Daily Surface Weather and Climatological Summaries
#' \item 'gridmet':   Gridded Meteorological Data; Multivariate Adaptive Constructed Analogs
#' \item 'prism':     Parameter-elevation Regressions on Independent Slopes
#' \item 'ncep':      National Centers for Environmental Predictions
#' \item 'topowx':    Topoclimatic Daily Air Temperature Dataset
#' \item 'loca':      Localized Constructed Analogs
#' \item 'bcca':      Statistically downscaled GCM data using Bias Corrected Constructed Analogs
#' \item 'sarrd_t':   Statistical Asynchronous Regional Regression Daily Downscaled Climate Projections Temperture
#' \item 'sarrd_pr':  Statistical Asynchronous Regional Regression Daily Downscaled Climate Projections Precipitation
#' \item 'BCSDhydro': Bias Corrected Spatially Downscaled - CMIP5 Monthly Hydrology Projections
#'
#' }
#' @examples
#' \dontrun{
#'  grids = climateR::grids
#' }
#'

"grids"

#' Model Metadata
#'
#' Metadata for model options
#'
#' @docType data
#' @examples
#' \dontrun{
#'  model_meta = climateR::model_meta
#' }
#'

"model_meta"



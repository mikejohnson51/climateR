#' Grids
#'
#' Grid meta data (Min/Max XY, dx/dy, proj) used for constructing subsetting calls to OPeNDAP.
#'
#' @docType data
#'
#' @format a \code{data.frame} contained source info for
#' \itemize{
#' \item 'daymet':    Daily Surface Weather and Climatological Summaries
#' \item 'gridmet':   Gridded Meteorological Data; Multivariate Adaptive Constructed Analogs
#' \item 'prism':     Parameter-elevation Regressions on Independent Slopes
#' \item 'loca':      Localized Constructed Analogs
#' \item 'terraclim':      Localized Constructed Analogs
#' \item 'maca':      Localized Constructed Analogs
#' }
#' @examples
#' \dontrun{
#'  grids = climateR::grid_meta
#' }
#'

"grid_meta"

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



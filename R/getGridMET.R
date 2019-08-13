#' @title Get GridMet Climate Data for an Area of Interest
#' @description gridMET is a dataset of daily high-spatial resolution (~4-km, 1/24th degree) surface meteorological data covering the contiguous US from 1979-yesterday.
#' These data are updated daily.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$gridmet`)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export

getGridMET = function(AOI, param, startDate, endDate = NULL){

  id = 'gridmet'
  base = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_'

  d = define.dates(startDate, endDate, baseDate = '1979-01-01')
  p = define.param(param, service = 'gridmet')
  g = define.grid3(AOI, id)

  urls = paste0(base, p$call, '_1979_CurrentYear_CONUS.nc?', p$description,
                '[', min(d$date.index), ':1:', max(d$date.index), "]",
                g$lat.call,  g$lon.call)

  s = fast.download(urls, params = p$description, names = p$common.name,  g, d$date, dataset = id, fun = 't')

  return(s)

}


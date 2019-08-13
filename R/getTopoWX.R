#' @title Get TopoWX Climate Data for an Area of Interest
#' @description The TopoWx ("Topography Weather") dataset contains historical 30-arcsec resolution (~800-m) interpolations of daily minimum and maximum topoclimatic air temperature for the conterminous U.S. 
#' Using both DEM-based variables and MODIS land skin temperature as predictors of air temperature, interpolation procedures include moving window regression kriging and geographically weighted regression. 
#' To avoid artificial climate trends, all input station data are homogenized using the GHCN/USHCN Pairwise Homogenization Algorithm (https://www.ncdc.noaa.gov/oa/climate/research/ushcn/#phas). 
#' The following data are available in this archive: 1948-2016 daily and monthly minimum and maximum temperature.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$topox`)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @param timeRes daily or monthly
#' @author Mike Johnson
#' @return a list of rasterStacks
#' @export

getTopoWX = function(AOI, param, startDate, endDate = NULL, timeRes = 'daily'){
  
  id = 'topowx'
  base =  "https://cida.usgs.gov/thredds/dodsC/topowx"
  
  if(!timeRes %in% c('daily', 'monthly')){ stop("timeRes must be monthly or daily") }
  
  d = define.dates(startDate, endDate, baseDate = "1948-01-01")
  p = define.param(param, service = id)

  if(timeRes == "monthly"){ 
    base = paste0(base, "_monthly?"); index = d$month.index 
  } else {
    base = paste0(base, "?"); index = d$date.index
  }
  
  g = define.grid3(AOI, source = id)
  
  urls = paste0(base, p$call,
                '[', min(index), ':1:', max(index), "]",
                g$lat.call,  g$lon.call)
  
  s = fast.download(urls, params = p$call, names = p$common.name,  g, d$date, dataset = id, fun = 't', scale_factor = .01)
  
  s
}


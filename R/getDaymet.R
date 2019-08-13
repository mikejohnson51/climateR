#' @title Get Daymet Climate Data for an Area of Interest
#' @description This dataset provides Daymet Version 3 model output data as gridded estimates of daily weather parameters for North America.
#' Daymet output variables include the following parameters: minimum temperature, maximum temperature, precipitation, shortwave radiation,
#' vapor pressure, snow water equivalent, and day length. The dataset covers the period from January 1, 1980 to December 31 of the most recent full calendar year. Each subsequent year is processed individually at the close of a calendar year after
#' allowing adequate time for input weather station data to be of archive quality.  Daymet variables are continuous surfaces provided as individual files, by year, at a 1-km x 1-km spatial resolution and a daily temporal resolution. Data are in a Lambert Conformal
#' Conic projection for North America and are in a netCDF file format compliant with Climate and Forecast (CF) metadata conventions.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$daymet`)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export
#'
getDaymet = function(AOI, param, startDate, endDate = NULL){

  id = 'daymet'
  base = 'https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1328/'

  d = define.dates(startDate, endDate, baseDate = "1980-01-01")
  d = d[d$julien < 366, ]
  p = define.param(param, id)
  g = define.grid3(AOI, id)

  y = data.frame(year = unique(d$year), minJul = NA, maxJul = NA, min.date = as.Date(NA), max.date = as.Date(NA), stringsAsFactors = FALSE)

  for(t in seq_along(y$year)){
    tmp = d[which(d$year == y$year[t]),]
    y$minJul[t]   = min(tmp$julien)
    y$maxJul[t]   = max(tmp$julien)
    y$min.date[t] = min(tmp$date)
    y$max.date[t] = max(tmp$date)
  }

  tmp = expand.grid(year = y$year, call = p$call, stringsAsFactors = FALSE)
  fin = merge(tmp, p, "call") %>% merge(y, "year")

  urls = paste0(base, fin$year, '/daymet_v3_',  fin$call, '_', fin$year, '_na.nc4?', fin$call,
                '[', fin$minJul - 1 ,   ':1:',  fin$maxJul - 1, ']',
                g$lat.call,
                g$lon.call)

  s = fast.download(urls, params = fin$call, names = fin$call, g, date.names =  d$date, dataset = id, fun = 't')

  s 
  
}


#' @title Get Terra Climate Data for an Area of Interest
#' @description TerraClim a dataset of high-spatial resolution (1/24Â°, ~4-km) monthly climate and climatic water balance for global terrestrial surfaces from 1958-2015.
#' These data were created by using climatically aided interpolation, combining high-spatial resolution climatological normals from the WorldClim version 1.4 and version 2 datasets, with coarser resolution time varying (i.e. monthly) data from CRU Ts4.0 and JRA-55
#' to produce a monthly dataset of precipitation, maximum and minimum temperature, wind speed, vapor pressure, and solar radiation. TerraClimate additionally produces monthly surface water balance datasets using a water balance model that incorporates reference
#' evapotranspiration, precipitation, temperature, and interpolated plant extractable soil water capacity.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$terraclim`)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export


getTerraClim = function(AOI, param, startDate, endDate = NULL){

  id = "terraclim"
  base = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_"

  d = define.dates  (startDate, endDate, baseDate = "1958-01-01")

  endDateYear = as.numeric(max(d$year))
  startDateYear =  as.numeric(min(d$year))

  if(endDateYear > 2018 | startDateYear < 1958){ stop("TerraClim data only avaliable between 1958 and 2018")}

  p = define.param  (param, service = id)
  g = define.grid3(AOI, id)

  urls = paste0(base, p$call, '_1958_CurrentYear_GLOBE.nc', "?", p$call,
                '[', min(d$month.index),':1:' ,max(d$month.index), ']',
                g$lat.call,
                g$lon.call)

  date.names  = unique(format(d$date, "%Y-%m"))

  s = fast.download(urls, params = p$call, names = p$common.name, g, date.names = date.names, dataset = id, fun = 't', no_data = 3000)

  s
  
  }


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

  d = define.dates(startDate, endDate, baseDate = '1979-01-01')

  p = define.param(param, service = id)
  g = define.grid(AOI, id)
  
  urls = list()
  date.names = list()
  
  for(i in 1:nrow(p)){
    
  if(p$timestep[i] == "pentad"){
    date.nc = paste0('[', min(d$pent_ind), ':1:', max(d$pent_ind), "]")
    date.names[[i]] = unique(paste0(d$year, "_pentad_", d$pentad))
  } else {
    date.nc = paste0('[', min(d$date.index), ':1:', max(d$date.index), "]")
    date.names[[i]] = d$date
  }
    
  urls[[i]] = paste0(g$base, p$call[i], '_1979_CurrentYear_CONUS.nc?', 
                     p$description[i],
                     date.nc, g$lat.call,  g$lon.call,
                     "#fillmismatch")
  }

  fast.download(urls   = unlist(urls),
                params = p$description, 
                names  = p$common.name,  
                g      = g, 
                date.names = date.names[[1]], 
                dataset = id, 
                fun = 't')


}


#' @title Get Terra Climate Data for an Area of Interest
#' @description TerraClim a data set of high-spatial resolution (1/24Â°, ~4-km) monthly climate and climatic water balance for global terrestrial surfaces from 1958-2015.
#' These data were created by using climatically aided interpolation, combining high-spatial resolution climatological normals from the WorldClim version 1.4 and version 2 datasets, with coarser resolution time varying (i.e. monthly) data from CRU Ts4.0 and JRA-55
#' to produce a monthly data set of precipitation, maximum and minimum temperature, wind speed, vapor pressure, and solar radiation. TerraClimate additionally produces monthly surface water balance datasets using a water balance model that incorporates reference
#' evapotranspiration, precipitation, temperature, and interpolated plant extractable soil water capacity.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meteorological parameter (see `param_meta$terraclim`)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export

getTerraClim = function(AOI, param, startDate, endDate = NULL){

  id = "terraclim"
  d = define.dates(startDate, endDate, baseDate = "1958-01-01")

  endDateYear = as.numeric(max(d$year))
  startDateYear =  as.numeric(min(d$year))

  if(endDateYear > 2020 | startDateYear < 1958){ stop("TerraClim data only avaliable between 1958 and 2020")}

  p = define.param(param, service = id)
  g = define.grid(AOI, id)

  urls = paste0(g$base, "_", p$call, '_1958_CurrentYear_GLOBE.nc', "?", p$call,
                '[', min(d$month.index),':1:' ,max(d$month.index), ']',
                g$lat.call,
                g$lon.call,
                "#fillmismatch")

  fast.download(urls, 
                params = p$call, 
                names = p$common.name, 
                g, 
                date.names = unique(format(d$date, "%Y-%m")), 
                dataset = id, 
                fun = 't', 
                no_data = 3000)

  

}


#' @title Get Terra Climate Normals for an Area of Interest
#' @description These layers from TerraClimate were creating using climatically aided interpolation of monthly anomalies from the CRU Ts4.0 and Japanese 55-year Reanalysis (JRA-55) datasets with WorldClim v2.0 climatologies.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$terraclim`). 
#' PDSI is not avalaible for any period, and "srad", "vap", "vpd" 
#' are only avaialble for '19812010' and  '19611990' 
#' @param period a period of record. Valid options 
#' include: '19812010' (default), '19611990', '4C' , and '2C'
#' @param month numeric. and month or vector of months to access. Default is 1:12
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export

getTerraClimNormals = function(AOI, param, period = '19812010' , month = 1:12){
  
  good.periods = c('19812010', '19611990', '4C' , '2C')
  
  if(!any(period %in% good.periods)){
    stop(paste('period must be one of:', paste(good.periods, collapse = ", ")))
  }
  
  if(any("PDSI" %in% param)){
    stop("While PDSI exists in the terraclim product, it is not represented in the normals")
  }
  
  if(any(c("srad", "vap", "vpd") %in% param & any(period %in% c('2C', '4C')))){
    stop("srad, vap, vpd are not avaialbe as normals for periods 2C and 4C ")
  }
  
  if(!any(month %in% 1:12)){ stop(paste('month must be 1-12'))}
  
  id = "terraclim"
  p  = define.param(param, service = id) 
  g  = define.grid(AOI, id)
  
  urls = paste0('http://thredds.northwestknowledge.net:8080/thredds/dodsC/',
         'TERRACLIMATE_ALL/summaries/TerraClimate',
          period,'_', p$call,'.nc?',
          p$call, '[', min(month) - 1, ':1:', max(month) - 1, ']',
          g$lat.call,
          g$lon.call,
          "#fillmismatch") 
  
  fast.download(urls, 
                params = rep(p$call, each = length(period)), 
                names = paste0(period,"_", p$common.name), 
                g, 
                date.names = paste0("X", sprintf("%02d", unique(month))), 
                dataset = id, 
                fun = 't', 
                no_data = 3000)
}

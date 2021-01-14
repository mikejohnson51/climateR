#' @title Get BCCA Climate Data for an Area of Interest
#' @description Bias Corrected Constructed Analogs (BCCA) V2 Daily Climate Projections.
#' This service offeres an interface containing daily BCCA CMIP3 and CMIP5 projections of precipitation, daily maximum, and daily minimum temperature over the contiguous
#' United States. The dataset content is based on global climate projections from the World Climate Research Programme's (WCRP's) Coupled Model
#' Intercomparison Project phase 3 (CMIP3) multi-model dataset referenced in the Intergovernmental Panel on Climate Change Fourth Assessment Report,
#' and the phase 5 (CMIP5) multi-model dataset that is informing the IPCC Fifth Assessment.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$bcca`)
#' @param model GMC model name (see `model_meta$bcca`)
#' @param scenario a climate scenario pathway (rcp45 or rcp85)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export

getBCCA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

  id = 'bcca'

  d = define.dates(startDate, endDate, baseDate = "1950-01-01", splitDate = "2006-01-01")
  v = define.versions(dates = d, scenario = scenario, future.call = "future?", historic.call = "historical?")
  p = define.param(param, service = id)
  k = define.config(dataset = id, model = model, ensemble = NA)

  tmp = expand.grid(min.date = v$min.date, model = k, call = p$call, stringsAsFactors = FALSE)
  fin = merge(v, tmp, "min.date")  %>% 
        merge(p, "call") %>%  
        merge(climateR::model_meta$bcca, "model")
  fin = fin[fin$scenario %in% scenario,]

  g = define.grid3(AOI, source = id)

  variable_call = paste0( "BCCA_0-125deg_",fin$call, "_day_", fin$model, "_", fin$ver, "_", fin$ensemble)

  urls = paste0(g$base, fin$calls, variable_call, fin$time.index, g$lat.call, g$lon.call)

  s = fast.download(urls, params = variable_call, names = paste0(fin$model, "_",fin$ensemble, "_", fin$call), g, date.names = d$date, dataset = id)

  s

}

#' @title Get BCCA Climate Data for an Area of Interest
#' @description Bias Corrected Constructed Analogs (BCCA) V2 Daily Climate Projections.
#' This archive contains daily BCCA CMIP3 and CMIP5 projections of precipitation, daily maximum, and daily minimum temperature over the contiguous
#' United States. The dataset content is based on global climate projections from the World Climate Research Programme's (WCRP's) Coupled Model
#' Intercomparison Project phase 3 (CMIP3) multi-model dataset referenced in the Intergovernmental Panel on Climate Change Fourth Assessment Report,
#' and the phase 5 (CMIP5) multi-model dataset that is informing the IPCC Fifth Assessment.
#' @param param a meterological parameter (see `param_meta$bcca`)
#' @param model GMC model name (see `model_meta$bcca$name`)
#' @param scenario a climate scenario pathway (rcp45 or rcp85)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return a list of rasterStacks
#' @export

getBCCA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

   base = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/'

  d = define.dates(startDate, endDate)
  v = define.versions(dates = d$date, scenario = scenario, future.call = "future?", historic.call = "historical?")
  p = define.param(param, service = 'bcca')
  k = define.config(dataset = "bcca", model = model, ensemble = NA)

  tmp = expand.grid(min.date = v$min.date, model = k, call = p$call, stringsAsFactors = FALSE)
  fin = merge(v, tmp, "min.date")  %>% merge(p, "call") %>%  merge(model_meta$bcca, "model")
  fin = fin[fin$scenario %in% scenario,]

  g = define.grid2(AOI, url = paste0(base, fin$calls[1]) )
  s = define.initial(g, d, p, dataset = "bcca")

  for(i in 1:NROW(fin)){

    variable_call = paste0( "BCCA_0-125deg_",fin$call[i], "_day_", fin$model[i], "_", fin$ver[i], "_", fin$ensemble[i])

      nc = RNetCDF::open.nc(paste0(
        base, fin$calls[i], variable_call,
                          fin$time.index[i],
                          g$lat.call,
                          g$lon.call))

      var = RNetCDF::var.get.nc(nc, variable_call )

      RNetCDF::close.nc(nc)

      s = process.var(group = s, g = g, var, dates = seq.Date(fin$min.date[i], fin$max.date[i],1),
                      param = fin$common.name[i], name = paste0(fin$ver[i], "_", fin$model[i]))
    }

  return(s)

}

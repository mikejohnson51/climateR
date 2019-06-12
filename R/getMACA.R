#' @title Get MACA Climate Data for an Area of Interest
#' @description Multivariate Adaptive Constructed Analogs (MACA) is a statistical method for downscaling Global Climate Models
#' (GCMs) from their native coarse resolution to a higher spatial resolution that catures reflects observed patterns of daily near-surface meteorology and simulated changes in GCMs experiments.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$maca`)
#' @param model GMC model name (see `model_meta$maca`)
#' @param scenario a climate scenario pathway (rcp45 or rcp85)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @param timeRes daily or monthly
#' @author Mike Johnson
#' @return a list of rasterStacks
#' @export

getMACA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL, timeRes = 'daily'){

  id = 'maca'
  base =  "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_"

  if(!timeRes %in% c('daily', 'monthly')){ stop("timeRes must be monthly or daily") }

  d = define.dates(startDate, endDate, baseDate = "1950-01-01", splitDate = "2006-01-01")
  v = define.versions(dates = d, scenario = scenario, future.call = paste0("2006_2099_CONUS_", timeRes, ".nc?"), historic.call = paste0("1950_2005_CONUS_", timeRes, ".nc?"), timeRes = timeRes)
  p = define.param(param, service = 'maca')
  k = define.config(dataset = "maca", model = model, ensemble = NA)

  tmp = expand.grid(min.date = v$min.date, model = k, call = p$call, stringsAsFactors = FALSE)
  fin = merge(v, tmp, "min.date")  %>% merge(p, "call") %>% merge(model_meta$maca, "model")

  if(timeRes == "monthly"){ name.date = format(d$date, "%Y-%m") } else {name.date = d$date}

  g = define.grid3(AOI, source = id)

  urls = paste0(base, fin$call, "_", fin$model, "_", fin$ensemble, "_", fin$ver, "_", fin$calls, fin$call2, fin$time.index, g$lat.call, g$lon.call)

  pp = paste0(fin$model,"_", fin$common.name)

  s = fast.download(urls, params = fin$call2, names = pp, g, name.date, dataset = id, fun = 'r')

  s

}


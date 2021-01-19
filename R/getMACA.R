#' @title Get MACA Climate Data for an Area of Interest
#' @description Multivariate Adaptive Constructed Analogs (MACA) is a statistical method for downscaling Global Climate Models
#' (GCMs) from their native coarse resolution to a higher spatial resolution that catures reflects observed patterns of daily near-surface meteorology and simulated changes in GCMs experiments.
#' @param AOI a spatial object (sf or sp)
#' @param param a meterological parameter (see `param_meta$maca`)
#' @param model GMC model name (see `model_meta$maca`)
#' @param scenario a climate scenario pathway (rcp45 or rcp85)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @param timeRes daily or monthly
#' @author Mike Johnson
#' @return a list of rasterStacks
#' @export

getMACA = function(AOI, param, 
                   model = 'CCSM4', scenario = 'rcp45', 
                   startDate, endDate = NULL, timeRes = 'daily'){

  id = 'maca'
  
  if(!timeRes %in% c('daily', 'monthly')){ stop("timeRes must be monthly or daily") }

  g = define.grid(AOI, source = id)
  d = define.dates(startDate, endDate, baseDate = "1950-01-01", splitDate = "2006-01-01")
  v = define.versions(dates = d, scenario = scenario, future.call = paste0("2006_2099_CONUS_", timeRes, ".nc?"), historic.call = paste0("1950_2005_CONUS_", timeRes, ".nc?"), timeRes = timeRes)
  p = define.param(param, service = id)
  k = define.config(dataset = id, model = model, ensemble = NA)

  fin = expand.grid(min.date = v$min.date, model = tolower(k), call = p$call, stringsAsFactors = FALSE) %>% 
    merge(v, "min.date")  %>% 
    merge(p, "call")
  
  tmp = climateR::model_meta$maca
  tmp$model2 = tolower(tmp$model)
  
  fin = merge(fin, tmp, by.x = "model", by.y =  "model2") 
  fin$scenario = NULL
  fin = fin[fin$ver %in% scenario,]
  
  fin = fin[!duplicated(fin),]

  if(timeRes == "monthly"){ name.date = format(d$date, "%Y-%m") } else {name.date = d$date}
  
  urls = paste0(g$base, fin$call, "_", 
                fin$model.y, "_", 
                fin$ensemble, "_", 
                fin$ver, "_", 
                fin$calls, fin$call2, 
                fin$time.index, 
                g$lat.call, g$lon.call)

  s = fast.download(urls, 
                    params = fin$call2, 
                    names = paste0(fin$model,"_", fin$common.name,"_", fin$ver, "_", fin$units), 
                    g, 
                    date.names = name.date, 
                    dataset = id, 
                    fun = 'r')

  s
}



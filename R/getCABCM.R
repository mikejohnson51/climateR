#' @title Get CABCM Climate Data for an Area of Interest
#' @description The Basin Characterization Model (BCM) dataset provides historical and projected climate and hydrology data at a 270 meter resolution,
#' which is relevant for watershed-scale evaluation and planning.
#' These data have formed the basis for multiple research projects and vulnerability assessments applying
#' climate change projections to conservation decision-making, providing a common base-layer and set of
#' assumptions across these projects.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$cabcm``)
#' @param model GMC model name (see `model_meta$cabcm`)
#' @param scenario a climate scenario pathway (see `model_meta$cabcm`)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export

getCABCM = function(AOI, param, model = 'CCSM4', scenario = 'rcp85', startDate, endDate = NULL){

  id = 'cabcm'
  
  g = define.grid(AOI, 
                  source = id)
  
  d = define.dates(startDate, endDate, 
                   baseDate = "1921-01-01", splitDate = "2010-01-01")
  
  v = define.versions(dates = d, 
                      scenario = scenario,
                      future.call = "future?", 
                      historic.call = "historical?", 
                      timeRes = "monthly")
  
  p = define.param(param, service = id)
  
  k = define.config(dataset = id, model = model, ensemble = NA)

  fin = expand.grid(min.date = v$min.date, 
                    model = k, 
                    call = p$call) %>% 
    merge(v, "min.date") %>% 
    merge(p, "call") %>% 
    merge(climateR::model_meta$cabcm, "model")

  urls = paste0(g$base, 
                fin$calls, 
                fin$model, "_", fin$scenario, "_Monthly_", 
                fin$call, fin$time.index, 
                g$lat.call, g$lon.call)

  fast.download(urls, 
                params = paste0(fin$model, "_", fin$scenario, "_Monthly_", fin$call), 
                names = paste0(fin$model,"_", fin$common.name), 
                g = g, 
                date.names = format(d$date, "%Y-%m"), 
                dataset = id, 
                fun = 'r')

}


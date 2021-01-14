#' @title Get LOCA Climate Data for an Area of Interest
#' @description LOCA is a statistical downscaling technique that uses past history to add improved fine-scale detail to global climate models.
#' LOCA has been used to downscale 32 global climate models from the CMIP5 archive at a 1/16th degree spatial resolution, covering North America from central Mexico through Southern Canada. The historical period is 1950-2005,
#' and there are two future scenarios available: RCP 4.5 and RCP 8.5 over the period 2006-2100 (although some models stop in 2099). The variables currently available are daily minimum and maximum temperature, and daily precipitation.
#' For more information visit: http://loca.ucsd.edu/.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$loca`)
#' @param model GMC model name (see `model_meta$loca`)
#' @param scenario a climate scenario pathway (rcp45 or rcp85)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export


getLOCA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

    id = 'loca'

    d = define.dates(startDate, endDate, baseDate = "1950-01-01", splitDate = "2006-01-01")
    v = define.versions(dates = d, scenario = scenario, future.call = "future?", historic.call = "historical?")
    p = define.param(param, service = id)
    g = define.grid3(AOI, source = id)
    k = define.config(dataset = id, model = model, ensemble = NA)

    tmp = expand.grid(min.date = v$min.date, model = k, call = p$call)
    fin = merge(v, tmp, "min.date")  %>% merge(p, "call") %>% 
        merge(climateR::model_meta$loca, "model")
    fin = fin[fin$scenario %in% scenario,]

    var.names = paste0(fin$call, "_", fin$model, "_", fin$ensemble, "_", fin$ver)

    urls = paste0(g$base, fin$calls, var.names,  fin$time.index, g$lat.call, g$lon.call)

    s = fast.download(urls, params = var.names, names = paste0(fin$model, "_", fin$common.name), g, date.names = d$date, dataset = id)

    s
}



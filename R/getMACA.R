#' @title Get MACA Climate Data for an Area of Interest
#' @description Multivariate Adaptive Constructed Analogs (MACA) is a statistical method for downscaling Global Climate Models
#' (GCMs) from their native coarse resolution to a higher spatial resolution that captures
#' reflects observed patterns of daily near-surface meteorology and simulated changes in GCMs experiments.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$maca`)
#' @param model GMC model name (see `model_meta$maca$name`)
#' @param scenario a climate scenario pathway (rcp45 or rcp85)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return a list of rasterStacks
#' @export

getMACA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

  d = define.dates(startDate, endDate)
  v = define.versions(dates = d$date, scenario = scenario, future.call = "2006_2099_CONUS_daily.nc?", historic.call = "historical_1950_2005_CONUS_daily.nc?")
  p = define.param(param, service = 'maca')
  k = define.config(dataset = "maca", model = model, ensemble = NA)

  tmp = expand.grid(min.date = v$min.date, model = k, call = p$call, stringsAsFactors = FALSE)

  fin = merge(v, tmp, "min.date")  %>%
        merge(p, "call") %>%
        merge(model_meta$maca, "model")

  base =  "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_"
  def  =  paste0(fin$call[1], "_", fin$model[1], "_", fin$ensemble[1], "_", fin$ver, "_")
  end  =  gsub("\\?", "", v$calls)

  g = define.grid2(AOI, url = paste0(base,def,end))
  s = define.initial(g, d, p, dataset = "maca")

for(i in 1:NROW(fin)){

    def  =  paste0(fin$call[i], "_", fin$model[i], "_", fin$ensemble[i], "_", fin$ver, "_")

    nc   = RNetCDF::open.nc(paste0(base, def, fin$calls[i], fin$call2[i], fin$time.index[i], g$lat.call, g$lon.call))

    var  = RNetCDF::var.get.nc(nc, fin$call2[i])

    RNetCDF::close.nc(nc)

    s = process.var(group = s,
                    g = g,
                    var,
                    dates = seq.Date(fin$min.date[i], fin$max.date[i],1),
                    param = fin$common.name[i], name = paste0(fin$ver[i], "_", fin$model[i])
                    )
  }

  return(s)

}

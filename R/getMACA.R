getMACA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

  d = define.dates(startDate, endDate)
  v = define.versions(dates = d$date, scenario = scenario, future.call = "2006_2099_CONUS_daily.nc?", historic.call = "historical_1950_2005_CONUS_daily.nc?")
  g = define.grid(AOI, service = 'gridmet')
  p = define.param(param, service = 'maca')
  s = define.initial(g, d, p, dataset = "maca")
  k = define.config(dataset = "maca", model = model, ensemble = NA)

  tmp = expand.grid(min.date = v$min.date, model = k, call = p$call)
  fin = merge(v, tmp, "min.date")  %>% merge(p, "call") %>%  merge(model_meta$loca, "model")
  fin = fin[fin$scenario %in% scenario,]

for(i in 1:NROW(fin)){

    base =  "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_"

    def  =  paste0(fin$call[i], "_", fin$model[i], "_", fin$ensemble[i], "_", fin$ver, "_")

    nc = ncdf4::nc_open(paste0(base, def, fin$calls[i], fin$call2[i], fin$time.index[i], g$lat.call, g$lon.call))

    var = ncdf4::ncvar_get(nc, fin$call2[i])

    ncdf4::nc_close(nc)

    s = process.var(group = s, g = g, var, dates = seq.Date(fin$min.date[i], fin$max.date[i],1),
                    param = fin$common.name[i], name = paste0(fin$ver[i], "_", fin$model[i]))
  }

  return(s)

}



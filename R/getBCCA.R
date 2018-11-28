getBCCA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

  d = define.dates(startDate, endDate)
  v = define.versions(dates = d$date, scenario = scenario, future.call = "future?", historic.call = "historical?")
  g = define.grid(AOI, service = 'bcca')
  p = define.param(param, service = 'bcca')
  s = define.initial(g, d, p, dataset = "bcca")
  k = define.config(dataset = "bcca", model = model, ensemble = NA)

  tmp = expand.grid(min.date = v$min.date, model = k, call = p$call)
  fin = merge(v, tmp, "min.date")  %>% merge(p, "call") %>%  merge(model_meta$bcca, "model")
  fin = fin[fin$scenario %in% scenario,]

  for(i in 1:NROW(fin)){

      nc = ncdf4::nc_open(paste0(
        "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/",
         fin$calls[i],
         "BCCA_0-125deg_",
                          fin$call[i],
                          "_day_",
                          fin$model[i], "_",
                          fin$ver[i], "_",
                          fin$ensemble[i],
                          fin$time.index[i],
                          g$lat.call,
                          g$lon.call))

      var = ncdf4::ncvar_get(nc)

      ncdf4::nc_close(nc)

      s = process.var(group = s, g = g, var, dates = seq.Date(fin$min.date[i], fin$max.date[i],1),
                      param = fin$common.name[i], name = paste0(fin$ver[i], "_", fin$model[i]))
    }

  return(s)

}

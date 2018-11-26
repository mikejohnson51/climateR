getTerraClim = function(AOI, param, startDate, endDate = NULL){

  d = define.dates  (startDate, endDate, baseDate = "1958-01-01")
  g = define.grid   (AOI, service = 'terraclim')
  p = define.param  (param, service = 'terraclim')
  s = define.initial(g, d)

  for(i in 1:NROW(p)){

    nc = ncdf4::nc_open(paste0(
      "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_",
      p$call[i],
     '_1958_CurrentYear_GLOBE.nc?',
      p$call[i],
      '[', min(d$month.index),':1:' ,max(d$month.index), ']',
      g$lat.call,
      g$lon.call
    ))

      var = ncdf4::ncvar_get(nc, p$call[i])
      var[var == 3276.8] = NA

      ncdf4::nc_close(nc)

    s = process.var(group = s, g = g, var, fun = 't', dates = d$date, param = param[i], proj = "+init=epsg:4269")
  }

  return(s)

}



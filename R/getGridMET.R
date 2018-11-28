getGridMET = function(AOI, param, startDate, endDate = NULL){

  d = define.dates(startDate, endDate, baseDate = '1979-01-01')
  g = define.grid(AOI, service = 'gridmet', proj = "+init=epsg:4269")
  p = define.param(param, service = 'gridmet')
  s = define.initial(grid = g, date = d)

  for(i in 1:NROW(p)){

    base = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_'

    call = paste0(p$call[i], '_1979_CurrentYear_CONUS.nc?', p$description[i])

    time = paste0('[', min(d$date.index), ':1:', max(d$date.index), "]")

    nc = ncdf4::nc_open(paste0(base, call, time, g$lat.call,  g$lon.call))

    var = ncdf4::ncvar_get(nc, p$description[i])

    ncdf4::nc_close(nc)

    s = process.var(group = s, g = g, var, dates = d$date, param = param[i], name = NULL)

    }

  return(s)

}

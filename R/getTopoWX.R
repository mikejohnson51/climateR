getTopoWX = function(AOI, param, startDate, endDate = NULL){

  d = define.dates(startDate, endDate, baseDate = '1948-01-01')
  g = define.grid(AOI,    service = 'topowx')
  p = define.param(param, service = 'topowx')
  s = define.initial(g,d)

  for(i in 1:NROW(p)){

    nc = ncdf4::nc_open(paste0('https://cida.usgs.gov/thredds/dodsC/topowx?',
                               p$call[i],
                               "[", min(d$date.index) , ":1:", max(d$date.index), "]",
                               g$lat.call,
                               g$lon.call))

    var = ncdf4::ncvar_get(nc)

    ncdf4::nc_close(nc)

    s = process.var(group = s, g = g, var, fun = 't', dates = d$date, param = param[i], name = NULL)
  }

  return(s)
}

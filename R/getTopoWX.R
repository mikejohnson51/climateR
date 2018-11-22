getTopoWX = function(AOI, param, startDate, endDate = NULL){

  d = define.dates(startDate, endDate, baseDate = '1948-01-01')
  g = define.grid(AOI,    service = 'topowx')
  p = define.param(param, service = 'topowx')
  s = define.initial(g,d)

  for(j in 1:NROW(p)){

    base = 'https://cida.usgs.gov/thredds/dodsC/topowx?'
    call = p$call[j]
    time = paste0("[", min(d$date.index) , ":1:", max(d$date.index), "]")

    nc = nc_open(paste0(base, call, time,
                        if(g$type == 'point'){paste0('[', g$y, ':1:', g$y, ']')} else {paste0('[', g$ymax, ':1:', g$ymin, ']') },
                        g$lon.call))

    var = ncvar_get(nc, call)

    nc_close(nc)

    s = process.var(group = s, g = g, var, fun = 't', dates = d$date, param = param[j])

  }

  if(g$type == 'grid') { s[['AOI']] = g$AOI }

  return(s)

}



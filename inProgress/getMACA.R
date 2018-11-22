getMACA = function(AOI, param, model, scenario, startDate, endDate = NULL){

  d = define.dates(startDate, endDate, baseDate = '2006-01-01')
  g = define.grid(AOI, service = 'gridmet')
  p = define.param(param, service = 'maca')
  s = define.initial(g, d)

for(i in 1:NROW(p)){

  base =  "https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future?"

  call  =  paste0(p$call, "_", model,ifelse(model == 'CCSM4', '_r6i1p1_', '_r1i1p1_') , scenario)

  time =  paste0("[", min(d$date.index) , ":1:", max(d$date.index), "]")

  nc = nc_open(paste0(base, call, time, g$lat.call, g$lon.call))

  var = ncvar_get(nc, call)

  nc_close(nc)

  s = process.var(group = s, g = g, var, dates = d$date, param = param[i])

 }

  if(g$type == 'grid') { s[['AOI']] = g$AOI }

  return(s)

}





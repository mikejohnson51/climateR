getSARRD = function(AOI, param, model = 'CCSM', ensemble = 'a2', startDate = NULL, endDate = NULL){

  config = define.config(dataset = "sarrd",  model = model, ensemble = ensemble)
  d    = define.dates(startDate, endDate, baseDate = '1959-12-01')
  g_pr = define.grid(AOI,   service = 'sarrd_pr')
  g_t  = define.grid( AOI,    service = 'sarrd_t')
  p    = define.param(param, service = 'sarrd')
  s    = define.initial(g_t, d)

  fin = expand.grid(model = config$model, ensemble = config$ensemble, param = p$call)
  fin = fin[!duplicated(fin),]

  for(i in 1:NROW(fin)){

   if(fin$param[i] == "pr"){
      call = 'pr'
      grid_curr = g_pr
    } else {
      call = 't'
      grid_curr = g_t}

  nc = ncdf4::nc_open(paste0("https://cida.usgs.gov/thredds/dodsC/dcp/conus_", call ,"?",
    tolower(fin$model[i]),
    "-",
    fin$ensemble[i],
    "-",
    fin$param[i],
    "-",
    "NAm-grid",
    "[", min(d$date.index), ":1:" , max(d$date.index), "]",
    grid_curr$lat.call,
    grid_curr$lon.call)
  )

  var = ncdf4::ncvar_get(nc)
  ncdf4::nc_close(nc)

  s = process.var(group = s, g = grid_curr, var,  dates = d$date, param = paste0(fin$param[i], "_", tolower(fin$model[i]), "_", fin$ensemble[i]))

  }
  return(s)
}


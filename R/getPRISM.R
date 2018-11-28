getPRISM = function(AOI, param, startDate, endDate = NULL){

  d = define.dates  (startDate, endDate)
  g = define.grid   (AOI, service = 'prism')
  p = define.param  (param, service = 'prism')
  s = define.initial(g, d)

  tmp = expand.grid(string = d$string, call = p$call)
  fin = merge(tmp, p, "call") %>% merge(d, "string")

  for(i in 1:NROW(fin)){

      base = 'http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/'

      call = paste0(fin$year[i], '/PRISM_combo_', fin$string[i], '.nc?', fin$call[i], '[0:1:0]')

      nc = ncdf4::nc_open(paste0(base, call, g$lat.call,  g$lon.call))

      var = ncdf4::ncvar_get(nc)

      ncdf4::nc_close(nc)

      s = process.var(group = s, g = g, var, fun = 't', dates = fin$date[i],
                      param = fin$common.name[i], name = fin$date[i], proj = "+init=epsg:4269")
  }

  ss = define.initial(grid = g, date = d)

  for(i in 1:NROW(p)){ ss[[p$common.name[i]]] = raster::stack(s[grepl(p$common.name[i], names(s))]) }

  return(ss)
}


AOI = getAOI(state = "CO") %>% getPRISM(param = c('tmax', 'prcp'), startDate = "2017-12-30", endDate = '2018-01-10')

rasterVis::levelplot(AOI$tmax)

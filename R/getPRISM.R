getPRISM = function(AOI, param, startDate, endDate = NULL){

  d = define.dates  (startDate, endDate)
  g = define.grid   (AOI, service = 'prism')
  p = define.param  (param, service = 'prism')
  s = define.initial(g, d)

  for(i in 1:NROW(p)){

    tmp = list()

    for(j in 1:NROW(d)){

      base = 'http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/'

      call = paste0(d$year[j], '/PRISM_combo_', d$string[j], '.nc?', p$call[i])

      time = '[0:1:0]'

      nc = ncdf4::nc_open(paste0(base, call, time, g$lat.call,  g$lon.call))

      var = ncdf4::ncvar_get(nc, p$call[i])

      ncdf4::nc_close(nc)

      tmp[[j]] = var
    }

      if(g$type =='point'){
        var = do.call(c, tmp)
      } else {
        var = array(unlist(tmp), dim = c((g$xmax - g$xmin) + 1, (g$ymax - g$ymin) + 1, length(tmp)))
      }

    s = process.var(group = s, g = g, var, fun = 't', dates = d$date, param = param[i], proj = "+init=epsg:4269")
  }

    if(g$type == 'grid') { s[['AOI']] = g$AOI }

    return(s)
}

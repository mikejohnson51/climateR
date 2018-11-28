getDaymet = function(AOI, param, startDate, endDate = NULL){

  daymet.proj = "+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +a=6378137 +rf=298.257223563 +lat_1=25 +lat_2=60 +datum=WGS84"

  d = define.dates(startDate, endDate)
  g = define.grid(AOI, service = 'daymet', proj = daymet.proj)
  p = define.param(param, service = 'daymet')
  s = define.initial(grid = g, date = d)

  y = data.frame(year = unique(d$year), minJul = NA, maxJul = NA, min.date = as.Date(NA), max.date = as.Date(NA), stringsAsFactors = FALSE)

  for(t in seq_along(y$year)){
    tmp = d[which(d$year == y$year[t]),]
    y$minJul[t]   = min(tmp$julien)
    y$maxJul[t]   = max(tmp$julien)
    y$min.date[t] = min(tmp$date)
    y$max.date[t] = max(tmp$date)
  }

  tmp = expand.grid(year = y$year, call = p$call, stringsAsFactors = FALSE)
  fin = merge(tmp, p, "call") %>% merge(y, "year")

  for(i in 1:NROW(fin)){

      nc = ncdf4::nc_open(paste0('https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1328/',
                          fin$year[i],
                          '/daymet_v3_',
                          fin$call[i],
                          '_',
                          fin$year[i],
                          '_na.nc4?',
                          fin$call[i],
                          '[', fin$minJul[i] -1 ,   ':1:', fin$maxJul[i] -1, ']',
                          g$lat.call,
                          g$lon.call ))

      var = ncdf4::ncvar_get(nc)

      ncdf4::nc_close(nc)

      s = process.var(group = s, g = g, var, dates = seq.Date(as.Date(fin$min.date[i]), as.Date(fin$max.date[i]), 1),
                      param = fin$common.name[i], name = fin$year[i], proj = daymet.proj, fun = 't')
  }

  ss = define.initial(grid = g, date = d)

  for(i in 1:NROW(p)){ ss[[p$common.name[i]]] = raster::stack(s[grepl(p$common.name[i], names(s))]) }

  return(ss)
}



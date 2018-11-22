getDaymet = function(AOI, param, startDate, endDate = NULL){

  daymet.proj = "+proj=lcc +lon_0=-100 +lat_0=42.5 +x_0=0 +y_0=0 +a=6378137 +rf=298.257223563 +lat_1=25 +lat_2=60 +datum=WGS84"

  d = define.dates(startDate, endDate)
  g = define.grid(AOI, service = 'daymet', proj = daymet.proj)
  p = define.param(param, service = 'daymet')
  s = define.initial(grid = g, date = d)

  y = data.frame(year = unique(d$year), minJul = NA, maxJul = NA, min.date = as.Date(NA), max.date = as.Date(NA), stringsAsFactors = FALSE)

  for(t in seq_along(y$year)){
    tmp = d[which(d$year == y$year[t]),]
    y$minJul[t] = min(tmp$julien)
    y$maxJul[t] = max(tmp$julien)
    y$min.date[t] = min(tmp$date)
    y$max.date[t] = max(tmp$date)
  }

  for(j in 1:NROW(p)){

    tmp = define.initial(grid = g, date = d)
    for(i in 1:NROW(y)){

      nc = nc_open(paste0('https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/',
                          '1328',
                          '/',
                          y$year[i],
                          '/daymet_v3_',
                          p$call[j],
                          '_',
                          y$year[i],
                          '_na.nc4?',
                          p$call[j],
                          '[', y$minJul[i] -1 ,   ':1:', y$maxJul[i] -1, ']',
                          g$lat.call,
                          g$lon.call ))

      var = ncvar_get(nc, p$call[j])

      nc_close(nc)

      dd = seq.Date(as.Date(y$min.date[i]), as.Date(y$max.date[i]), 1)
      tmp = process.var(group = tmp, g = g, var, dates = dd, param = paste0(y$year[i], "_", p$call[j]), proj = daymet.proj, fun = 't')
    }

    index = which(grepl(p$call[j], names(tmp)))
    l = list()

    for(y in seq_along(index)){ l[[y]] = tmp[[y]]}
    s[[p$call[j]]] = raster::stack(l)

  }

  if(g$type == 'grid') { s[['AOI']] = g$AOI }
  return(s)
}



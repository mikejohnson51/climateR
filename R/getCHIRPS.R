getCHIRPS = function(AOI, startDate, endDate = NULL  ){

  d = define.dates  (startDate, endDate)

  bb = AOI::bbox_st(AOI)

  dates = paste0("%28", as.numeric(format(d$date, "%d")), "%20", month.abb[as.numeric(format(d$date, "%m"))], "%20", format(d$date, "%Y"))

  urls = paste0('https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp/',
               'Y/', bb$ymin, '/', bb$ymax,'/RANGEEDGES/',
               'X/',bb$xmin, '/', bb$xmax,'/RANGEEDGES/',
               'T/',dates,'%29VALUES/',
               '%5BX/Y/%5D/palettecolor.tiff?filename=tmp.tiff')

  s = raster::stack()

  for(i in 1:length(urls)){
    r = raster::raster(urls[i])
    names(r) = paste0("prcp_", d$string[i])
    s = raster::addLayer(s, r )
  }
  return(s)
}

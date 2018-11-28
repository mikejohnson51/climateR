
startDate = "2017-12-30"
endDate = "2018-01-02"

'https://nomads.ncdc.noaa.gov/thredds/catalog/cfsr1hr/201009/catalog.html?dataset=CFSR1hrts/201009/soilm2.gdas.201009.grb2'

AOI = getAOI(state = 'FL')

getNCEP = function(AOI, param, startDate, endDate){
  s = list()

  if(is.null(endDate)){ endDate = startDate}
  g = define.grid(AOI, service = 'ncep')

  d.long = seq.Date(as.Date(startDate), as.Date(endDate), 1)

  x = data.frame(
    year = format(as.Date(d.long),'%Y'),
    index.1 = (as.numeric(format(as.Date(d.long),'%j'))*4) - 4,
    index.2 = (as.numeric(format(as.Date(d.long),'%j'))*4) - 3,
    index.3 = (as.numeric(format(as.Date(d.long),'%j'))*4) - 2,
    index.4 = (as.numeric(format(as.Date(d.long),'%j'))*4) - 1,
    stringsAsFactors = FALSE

  )

  url = paste0('http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis/surface_gauss/',
  'air.2m',
  '.gauss.',
  '2017',
  '.nc?',
  'air',
  '[', 0, ':1:', 1, ']',
  '[', g$ymax, ':1:', g$ymin, ']',
  '[', g$xmin, ':1:', g$xmax, ']'
  )

 nc = ncdf4::nc_open(url)
 var = ncdf4::ncvar_get(nc, "air")
 tmp = list()

 if(length(dim(var)) == 3){
   for( j in 1:dim(var)[3]){

     r  = raster::raster(apply(t(var[,,j]), 2, rev))
     raster::extent(r) <- g$e

     m <- raster::rasterize(AOI, r, getCover=TRUE)
     m[m==0] <- NA

     #m = raster::mask(r, AOI, snap = 'out')
     tmp[[paste0('X', d.long[j])]] = m
   }

   tmp = raster::stack(tmp)
   raster::crs(tmp) = "+init=epsg:4269"
   s[[param[i]]] = tmp

 } else {
   r  = raster::raster(apply(t(var), 2, rev))
   raster::extent(r) <- g$e
   m = raster::mask(r, AOI, snap = 'out')
   raster::crs(m) = "+init=epsg:4269"
   names(m) = paste0('X', d.long[1])
   s[[param[i]]] = m
 }
}



}


# grid = getAOI(list('Colorado Springs', 100, 100)) %>% getLOCA(param = 'tmax', model = 2, startDate = "2030-01-03")
system.time({
  ts = geocode('Colorado Springs') %>% getLOCA(param = 'tmax', model = 10, startDate = "2030-01-01", endDate = "2040-12-31")
})

length(ts$tmax$rcp45_CanESM2) * 10 / 193.675
user  system elapsed
2.716   4.506 193.675

# ts$tmax$`rcp45_ACCESS1-0` = ts$tmax$`rcp45_ACCESS1-0` - 273.15
# ts$tmax$`rcp45_ACCESS1-3` = ts$tmax$`rcp45_ACCESS1-3` - 273.15



mean = rowMeans(ts$tmax[,c(5:14)])
matplot(y = ts$tmax[,c(5:14)], type = "l", col = sf::sf.colors(10), axes = F, xlab = "Date", ylab = "Max Temp (C)")
title("LOCA 10-model ensemble of Max Temp\nColorado Springs, Colorado")
axis(side = 1, at=seq(1, NROW(ts$tmax), 20), labels= ts$tmax$date[seq(1,NROW(ts$tmax), 20)], las = 1, cex = .0001)
axis(side = 2, at     =      seq(min(ts$tmax[[6]]), max(ts$tmax[[6]]), 10),
     labels= round(seq(min(ts$tmax[[6]]), max(ts$tmax[[6]]), 10)), las = 2)
lines(mean, lwd =2, col = 'black')
lines(lowess(ts$tmax$date, mean)$y, col="darkred", lwd = 4)

legend(0, -16, legend=c("Ensemble Mean", "Lowess Trend"),
       col=c("black", "darkred"), lty=1, cex=0.8, lwd = c(2,4),
       box.lty=0)

head(ts$tmax)

dim(ts$tmax)


plot(ts$tmax$`rcp45_HadGEM2-AO`, type = 'l')
lines(ts$tmax$`rcp45_MPI-ESM-LR`, type = 'l', col = 'red')
plot(stack(grid))

getOSSEB = function(AOI, startDate, endDate = NULL){

d = define.dates  (startDate, endDate, baseDate = "2001-01-01")
g = define.grid   (AOI, service = 'osseb')
s = define.initial(g, d)

nc = ncdf4::nc_open(paste0("https://cida.usgs.gov/thredds/dodsC/ssebopeta/monthly?et[",
       min(d$month.index),":1:", max(d$month.index) ,"]",
       g$lat.call,
       g$lon.call))

    var = ncdf4::ncvar_get(nc, "et")

    ncdf4::nc_close(nc)

  s = process.var(group = s, g = g, var, fun = 'r', dates = d$date, param = "et", proj = "+init=epsg:4269")

  return(s)
}

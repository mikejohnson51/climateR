getMACA = function(AOI, param, model, scenario, sdate, edate = NULL, write = NULL){

base = "https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future?"

if(is.null(edate)){edate = sdate}
dates = seq.Date(as.Date(sdate, format = "%Y-%m-%d", tz = 'GMT' ), as.Date(edate, format = "%Y-%m-%d", tz = 'GMT' ) , by = 1)
dates = dates - as.Date("2006-01-01", format = "%Y-%m-%d", tz = 'GMT')

nc = ncdf4::nc_open(paste0(base, "lon,lat"))
lat = ncdf4::ncvar_get(nc, "lat")
lon = ncdf4::ncvar_get(nc, "lon")
ncdf4::nc_close(nc)

min.lat = which.min(abs(lat-min(AOI@bbox[2,])))
max.lat = which.min(abs(lat-max(AOI@bbox[2,])))

min.lon = which.min(abs(lon-min(AOI@bbox[1,])))
max.lon = which.min(abs(lon-max(AOI@bbox[1,])))

l = list(base =  paste0(param, "_",
                model, "_r1i1p1_",
                scenario),

         call =  paste0("[", dates[1] , ":1:", tail(dates, 1), "]",
                "[", min.lat , ":1:", max.lat, "]",
                "[", min.lon , ":1:", max.lon, "]")
)

xx = paste(do.call(paste0,l), collapse = ",")

xx = paste0( xx , ",time[",dates[1], ":1:", tail(dates, 1), "],lat[", min.lat , ":1:", max.lat, "],", "lon[", min.lon , ":1:", max.lon, "]")

r = ncdf4::nc_open(paste0(base, xx))

if(!is.null(write)) { write_nc(r, write_path = write)} else {
  ncdf4::nc_close(r)
  return(r)
}
}













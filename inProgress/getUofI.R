getUofI = function(AOI, param, sdate, edate = NULL, write.path = "/Users/mikejohnson/Documents/WRF_Hydro/output/"){

  base = "https://cida.usgs.gov/thredds/dodsC/UofIMETDATA?"

   nc = ncdf4::nc_open(paste0(base, "lon,lat"))
   lat = ncdf4::ncvar_get(nc, "lat")
   lon = ncdf4::ncvar_get(nc, "lon")


  if(is.null(edate)){edate = sdate}
  dates = seq.Date(as.Date(sdate, format = "%Y-%m-%d", tz = 'GMT' ), as.Date(edate, format = "%Y-%m-%d", tz = 'GMT' ) , by = 1)
  dates = dates - as.Date("2006-01-01", format = "%Y-%m-%d", tz = 'GMT')


  min.lat = which.min(abs(lat - min(AOI@bbox[2,])))
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

  r = ncdf4::nc_open(paste0(base, "time[0:1:0],", xx))

  return(r)

}














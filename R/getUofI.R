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

r = getMACA(AOI, param = c('rsds', 'uas'), model = 'GFDL-ESM2M', scenario = 'rcp85', sdate = '2020-10-29')


rr = getRaster(r)

raster::plot(rr)



sdate = '2020-11-01'
edate = '2020-11-02'



ncdf4::ncvar_get(r, paste0('uas_GFDL-ESM2M_r1i1p1_rcp85'))

r$var$`rsds_GFDL-ESM2M_r1i1p1_rcp85`


climate$macaV2metdata$models$name

rasterVis::levelplot(raster::raster(r))

#### Create New NetCDF file for current data ####

xvals = lon[c(min.lon:max.lon)]
yvals = lat[c(min.lat:max.lat)]

nx <- length(xvals)
ny <- length(yvals)

lon1 <- ncdim_def("longitude", "degrees_east", xvals)
lat2 <- ncdim_def("latitude", "degrees_north", yvals)

time <- ncdim_def("Time","days from 2006-01-01", as.numeric(sdate), unlim=TRUE)
mv <- -9999

var_temp <- ncvar_def("temperature", "mm", list(lon1, lat2, time), longname="Average daily temperature near surface", mv)
var_huss <- ncvar_def("specific_humidity", "kg kg-1", list(lon1, lat2, time), longname="Surface specific humidity", mv)
var_uas <- ncvar_def("eastward_wind", "m s-1", list(lon1, lat2, time), longname="Surface (10m) eastward wind", mv)
var_vas <- ncvar_def("northward_wind", "m s-1", list(lon1, lat2, time), longname="Surface (10m) northward windy", mv)
var_sw <- ncvar_def("downward_sw", "W m-2", list(lon1, lat2, time), longname="SW radiation incident at the surface", mv)
var_pr <- ncvar_def("precipiation", "mm", list(lon1, lat2, time), longname="Total daily precipitation at surface; includes both liquid and solid phases from all types of clouds (both large-scale and convective)", mv)

dir = paste0(write.path, "/", model, "/", scenario)
if(!dir.exists(dir)){dir.create(dir, recursive = T)}

path = paste0(dir, "/", m.date, "_macaV2metdata.nc")

ncnew <- nc_create(path, list(var_pr, var_huss, var_uas, var_vas, var_temp, var_sw))

tmax_data = ncdf4::ncvar_get(r, paste0('tasmax_', model, "_r1i1p1_", scenario))
tmin_data = ncdf4::ncvar_get(r, paste0('tasmin_', model, "_r1i1p1_", scenario))
tavg_data = (tmax_data + tmin_data) / 2
ncvar_put(ncnew, var_temp, tavg_data, start=c(1,1,1), count=c(nx,ny,1))
rm(tmax_data)
rm(tmin_data)
rm(tavg_data)

huss_data = ncdf4::ncvar_get(r, paste0('huss_', model, "_r1i1p1_", scenario))
ncvar_put(ncnew, var_huss, huss_data, start=c(1,1,1), count=c(nx,ny,1))
rm(huss_data)

uas_data = ncdf4::ncvar_get(r, paste0('uas_', model, "_r1i1p1_", scenario))
ncvar_put(ncnew, var_uas, uas_data, start=c(1,1,1), count=c(nx,ny,1))
rm(uas_data)

vas_data = ncdf4::ncvar_get(r, paste0('vas_', model, "_r1i1p1_", scenario))
ncvar_put(ncnew, var_vas, vas_data, start=c(1,1,1), count=c(nx,ny,1))
rm(vas_data)

sw_data = ncdf4::ncvar_get(r, paste0('rsds_', model, "_r1i1p1_", scenario))
ncvar_put(ncnew, var_sw, sw_data, start=c(1,1,1), count=c(nx,ny,1))
rm(sw_data)

pr_data = ncdf4::ncvar_get(r, paste0('pr_', model, "_r1i1p1_", scenario))
ncvar_put(ncnew, var_pr, pr_data, start=c(1,1,1), count=c(nx,ny,1))
rm(pr_data)

ncatt_put(ncnew, 0, "model_name", "macav2metdata_daily_future")
ncatt_put(ncnew, 0, "model_date", m.date)
ncatt_put(ncnew, 0, "reference", "https://climate.northwestknowledge.net/MACA/MACAproducts.php")
ncatt_put(ncnew, 0, "Origin", base)
ncatt_put(ncnew, 0, "Compiled", paste("Mike Johnson, UCSB", date(), sep = ", "))

nc_close(ncnew)

}




d = seq.Date(as.Date("2006-01-01"), as.Date("2006-12-31"), by = 1)

for(i in 1:length(d)){

  message("Starting ", d[i])

  getMACA(AOI = AOI,
          param = c('tasmax', 'tasmin', 'huss', 'pr', 'rsds', 'uas', 'vas'), model = "BNU-ESM",
          scenario = "rcp45",
          sdate = d[i],
          edate = NULL)

  message("Finished ", d[i])

}













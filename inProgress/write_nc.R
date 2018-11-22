write_nc = function(r, write_path, overwrite = F){

n = names(r$var)

model = strsplit(n[1], "_")[[1]][2]
scenario = strsplit(n[1], "_")[[1]][4]

xvals = r$dim$lon$vals
yvals = r$dim$lat$vals

nx <- length(xvals)
ny <- length(yvals)

lon1 <- ncdf4::ncdim_def("longitude", "degrees_east", xvals)
lat2 <- ncdf4::ncdim_def("latitude", "degrees_north", yvals)

time <- ncdf4::ncdim_def("Time","days from 2006-01-01", as.numeric(r$dim$time$vals), unlim=TRUE)
mv <- -9999

nc.vars = list()

for(i in 1:length(n)){

  nn = strsplit(n[i], "_")[[1]][1]

  data = climate$macaV2metdata$PARAM[which(climate$macaV2metdata$PARAM$parameter %in% nn),]

  nc.vars[[paste0("var_", n[i])]] = ncdf4::ncvar_def(data$longname, data$units, list(lon1,lat2,time), longname = "", mv )

}

dir = paste0(write_path, "/", model, "/", scenario)
if(!dir.exists(dir)){dir.create(dir, recursive = T)}

path = paste0(dir, "/", (as.Date("1900-01-01", format = "%Y-%m-%d") + as.numeric(r$dim$time$vals)),  "_macaV2metdata.nc")

if(all(file.exists(path), !overwrite)){ NULL} else {

ncnew <- ncdf4::nc_create(path, nc.vars)

for(i in 1:length(nc.vars)){
  d = ncdf4::ncvar_get(r, n[i])
  ncdf4::ncvar_put(ncnew, nc.vars[[i]], d, start=c(1,1,1), count=c(nx,ny,1))
  rm(d)
}

ncdf4::ncatt_put(ncnew, 0, "model_name", "macav2metdata_daily_future")
ncdf4::ncatt_put(ncnew, 0, "reference", "https://climate.northwestknowledge.net/MACA/MACAproducts.php")
ncdf4::ncatt_put(ncnew, 0, "Origin", "https://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future?")
ncdf4::ncatt_put(ncnew, 0, "Compiled", paste("Mike Johnson, UCSB", date(), sep = ", "))

ncdf4::nc_close(ncnew)
ncdf4::nc_close(r)
gc()

}
}

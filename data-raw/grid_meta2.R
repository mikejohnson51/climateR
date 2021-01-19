library(RNetCDF)
library(ncmeta)

## code to prepare `grid_meta` dataset goes here

process_coords = function(baseURL, test, names = c("lon", "lat", "day"), source = NULL, proj = NULL){
  url  = paste0(test, "#fillmismatch")
  if(is.null(proj)){ proj = '+proj=longlat +datum=WGS84 +no_defs'}
  nc   = RNetCDF::open.nc(url)
  X    = RNetCDF::var.get.nc(nc, names[1])
  Y    = RNetCDF::var.get.nc(nc, names[2])
  time = RNetCDF::var.get.nc(nc, names[3])
  
  setNames(data.frame(source, proj, baseURL,
                      head(X,1), tail(X,1), head(Y, 1), tail(Y, 1), 
                      diff(X)[1], diff(Y)[1], 
                      length(X), length(Y), length(time)),
           c("source", "proj", "base", "Xstart", "Xend", "Ystart", "Yend", "dX", "dY", "nX", "nY", "nT"))
}



# BCCA
baseURL = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/'
test = 'https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/historical?latitude,longitude,time'
bcca = process_coords(baseURL, test, names = c("longitude", "latitude", "time"), source = "bcca")

# CABCM
baseURL = "https://cida.usgs.gov/thredds/dodsC/CA-BCM-2014/"
test = "https://cida.usgs.gov/thredds/dodsC/CA-BCM-2014/historical?x,y,time"
proj = '+proj=aea +lat_0=0 +lon_0=-120 +lat_1=34 +lat_2=40.5 +x_0=0 +y_0=-4000000'
cabcm = process_coords(baseURL, test, names = c("x", "y", "time"), source = "cabcm", proj = proj)

# CHIRPS
baseURL = "https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp/"
test = "https://iridl.ldeo.columbia.edu/SOURCES/.UCSB/.CHIRPS/.v2p0/.daily-improved/.global/.0p05/.prcp/T/%2831%20Jan%202001%29VALUES/%5BX/Y/%5D/palettecolor.tiff?filename=tmp.tiff"
proj = '+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0'

t = raster(test)
chirps = setNames(data.frame("chirps", proj, baseURL,
                             t(as.vector(extent(t))),
                             t(res(t)), 
                             t(dim(t)[1:2]), NA),
                  c("source", "proj", "base", "Xstart", "Xend", "Ystart", "Yend", "dX", "dY", "nX", "nY", "nT"))


# Daymet
baseURL = 'https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1328/'
test = 'https://thredds.daac.ornl.gov/thredds/dodsC/ornldaac/1328/1980/daymet_v3_dayl_1980_na.nc4?y,x,time'
proj = '+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs'
daymet = process_coords(baseURL, test, names = c("x", "y", "time"), source = "daymet", proj = proj)

# EDDI
baseURL = "ftp://ftp.cdc.noaa.gov/Projects/EDDI/CONUS_archive/data/"
url = "ftp://ftp.cdc.noaa.gov/Projects/EDDI/CONUS_archive/data/2001/EDDI_ETrs_03wk_20010131.asc"
tmp = tempfile()
suppressWarnings( httr::GET(url, httr::write_disk(tmp, overwrite = TRUE)) ) # suppressing Government Warning
t = raster::raster(tmp)
raster::crs(t) ="+proj=longlat +datum=WGS84 +no_defs"
eddi = setNames(data.frame("eddi", "+proj=longlat +datum=WGS84 +no_defs",  baseURL,
                           t(as.vector(extent(t))),
                           t(res(t)), 
                           t(dim(t)[1:2]), NA),
                c("source", "proj", "base", "Xstart", "Xend", "Ystart", "Yend", "dX", "dY", "nX", "nY", "nT"))

#gridmet
baseURL = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_'
test = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc"
gridmet = process_coords(baseURL, test, names = c("lon", "lat", "day"), source = "gridmet")

# LOCA
baseURL = 'https://cida.usgs.gov/thredds/dodsC/loca_'
test = 'https://cida.usgs.gov/thredds/dodsC/loca_future?lat,lon,time'
loca = process_coords(baseURL, test, names = c("lon", "lat", "time"), source = "loca")

# maca
baseURL = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_'
test = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_huss_BNU-ESM_r1i1p1_historical_1950_2005_CONUS_daily.nc'
maca = process_coords(baseURL, test, names = c("lon", "lat", "time"), source = "maca")

# PRISM
baseURL = 'http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/'
test = 'http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/2020/PRISM_combo_20200922.nc?longitude,latitude,t'
prism = process_coords(baseURL, test, names = c("longitude", "latitude", "t"), source = "prism")

# TerraClim
baseURL = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate'
test = 'http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_terraclimate_aet_1958_CurrentYear_GLOBE.nc'
terraclim = process_coords(baseURL, test, names = c("lon", "lat", "time"), source = "terraclim")

# TopoWX
baseURL = 'https://cida.usgs.gov/thredds/dodsC/topowx'
test = 'https://cida.usgs.gov/thredds/dodsC/topowx?lat,lon,time'
topowx = process_coords(baseURL, test, names = c("lon", "lat", "time"), source = "topowx")

grid_meta = dplyr::bind_rows(list(bcca, cabcm, chirps, daymet, eddi, gridmet, loca, maca, prism, terraclim, topowx))

usethis::use_data(grid_meta, overwrite = TRUE)


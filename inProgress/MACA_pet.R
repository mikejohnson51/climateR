download.url = function(url){

  destdir <- normalizePath(paste0(tempdir(), "/."))
  destfile <- paste0(destdir, "/", basename(url))

  if(!file.exists(destfile)){
    x = httr::GET(url = url, httr::write_disk(destfile, overwrite = T), httr::progress(type = "down"))
  } else {x = list(status_code = 200)}

  return(list(code = x$status_code, destfile = destfile))

}

build.files = function(core){
  if(!dir.exists(core)){ dir.create(core, recursive = TRUE) }
  if(!dir.exists(paste0(core, "/rcp45"))){ dir.create(paste0(core, "/rcp45"), recursive = TRUE) }
  if(!dir.exists(paste0(core, "/rcp85"))){ dir.create(paste0(core, "/rcp85"), recursive = TRUE) }

  build.var.files = function(path){
    if(!dir.exists(paste0(path, "/tasmax"))){ dir.create(paste0(path, "/tasmax"), recursive = TRUE) }
    if(!dir.exists(paste0(path, "/tasmin"))){ dir.create(paste0(path, "/tasmin"), recursive = TRUE) }
    if(!dir.exists(paste0(path, "/wind"))){ dir.create(paste0(path, "/wind"), recursive = TRUE) }
    if(!dir.exists(paste0(path, "/shum"))){ dir.create(paste0(path, "/shum"), recursive = TRUE) }
    if(!dir.exists(paste0(path, "/dwrad"))){ dir.create(paste0(path, "/dwrad"), recursive = TRUE) }
    if(!dir.exists(paste0(path, "/totrad"))){ dir.create(paste0(path, "/totrad"), recursive = TRUE) }
    if(!dir.exists(paste0(path, "/rhum"))){ dir.create(paste0(path, "/rhum"), recursive = TRUE) }
    if(!dir.exists(paste0(path, "/vp"))){ dir.create(paste0(path, "/vp"), recursive = TRUE) }
    if(!dir.exists(paste0(path, "/pet"))){ dir.create(paste0(path, "/pet"), recursive = TRUE) }
  }

  build.var.files(paste0(core, "/rcp45"))
  build.var.files(paste0(core, "/rcp85"))


}

#######################
core = '/Volumes/Seagate5tb/MACA_PET/'
build.files(core)
base = "http://thredds.northwestknowledge.net:8080/thredds/fileServer/MACAV2/"
var = c('tasmax', 'tasmin', 'huss', 'rsds', 'was')
model = c("bcc-csm1-1-m", "CanESM2", "CCSM4", "CNRM-CM5", "CSIRO-Mk3-6-0", "GFDL-ESM2M", "HadGEM2-CC365", "IPSL-CM5A-MR", "MRI-CGCM3", "NorESM1-M")
years = paste0(seq(2016, 2075,5), "-", seq(2016, 2075,5) + 4)
scenario = c('rcp45', 'rcp85')
end = "_CONUS_monthly.nc"

all = expand.grid(var = var, model = model, scenario = scenario, years = years, stringsAsFactors = F)

all$url = paste0(base, all$model, "/macav2metdata_", all$var, "_", all$model, ifelse(all$model == "CCSM4", "_r6i1p1_", "_r1i1p1_"), all$scenario, "_", gsub("-", "_", all$years), end )

ref = expand.grid(model = model, scenario = scenario, years = years, stringsAsFactors = F)

###########################

for(a in 1:1){
h = list()

xx = all[all$model == ref$model[a] ,]
xx = xx[xx$scenario == ref$scenario[a],]
xx = xx[xx$years == ref$years[a],]

for(i in 1:NROW(xx)){
 tmp = download.url(xx$url[i])
 if(tmp$code != 200){stop()} else { h[[var[i]]] = tmp$destfile}
}

#########################################

tmax = raster::stack(h$tasmax)
tmin = raster::stack(h$tasmin)
shum = raster::stack(h$huss)
srad = raster::stack(h$rsds)
wind = raster::stack(h$was)

lon <- raster::init(wind, 'x')
lat <- raster::init(wind, 'y')

elev = download.url("https://climate.northwestknowledge.net/METDATA/data/metdata_elevationdata.nc")
elev = raster::raster(elev$destfile)
extent(elev) = extent(wind)
crs(elev) = crs(wind)

PET.list = list()

for(i in 1:12){

  P0 = 101325 # Sea Level Standard Pressure (Pa)
  g  = 9.80665 #  Earth surfce gravitational acceleration (m/s2)
  cp = 1007 # Constant pressure specfic heat (j/(kg * K))
  L  = g / cp # Temperture lapse rate ( K/m )
  T0 = 288.15 # Sea Level Standard Temp (k)
  M  = 0.0289644 # Molar Mass of dry air (kg/mol)
  R0 = 8.31447 # Universal Gas Constant ( J / (mol*k))
  DtR = 0.0174533 # Degrees to Radians
  Gsc = 4.92      # Solar Constant
  sb = 4.901e-9 # stefan-boltzman constant (MJ K-4 m-2 d-1)

  ##############################################################################################################

  yday = as.numeric(format(as.Date(gsub("\\.", "-", gsub("X", "", names(tmax[[i]])))), "%j"))
  P = asin(.39795*cos(.2163108 + 2*atan(.9671396*tan(.00860*(yday-186)))))
  daylength = (24 - (24/pi)*acos( (sin(0.8333*pi/180) + sin(lat*pi/180)*sin(P)) / (cos(lat*pi/180)*cos(P) ) ) ) * 3600

  tair = (tmin[[i]] + tmax[[i]]) / 2

  dr                = 1 + .033 * cos( ((2*pi) / 365) * yday ) # relative distance to sun
  solar_declination = .409 * sin( ((((2*pi) / 365) * yday )) - 1.39)

  sin_declin = sin(solar_declination)
  cos_declin = cos(solar_declination)

  sunset_hour_angle     = acos(-1*tan(lat*DtR) * tan(solar_declination))
  sin_sunset_hour_angle = sin(sunset_hour_angle)


  Ra = (24/pi) * Gsc * dr * ( sunset_hour_angle * sin(lat*DtR) * sin_declin  + cos(lat*DtR) * cos_declin * sin_sunset_hour_angle)
  Rso = (.75 + 2e-5*elev) * Ra

  myFun = function(x) { ifelse(x >= 100, (P0 * (1 - ((L * x) / T0 )^( (g * M) / (R0 * L) ))), (P0 - 1.2 * x)) }
  pressure_Pa = overlay(elev,fun = myFun)
  ##############################################################################################################

  pressure_mb = pressure_Pa / 100

  es = 6.112 * exp((17.67 * (tair - 273.15)) / ((tair - 273.15) + 243.5))
  e = (shum[[i]] * pressure_mb) / (.378 * shum[[i]]+ .622)
  rh = e / es
  rh[rh > 1] <- 1
  rh[rh < 0] <- 0

  vp_mb = (es * rh) / 100
  vp_kPa = vp_mb / 10

  # Cloudiness Function  ----------------------------------------------------

  Rs_mj = srad[[i]] * .0864 # Go Wm2 to MJ

  Fcd = 1.35 * ( Rs_mj / Rso ) - .35
  Fcd[Fcd > 1]   <- 1
  Fcd[Fcd < .05] <- .05

  # Net Long-Wave Radiation -------------------------------------------------

  Rnl_mj = sb * Fcd * (.34 - .014 * sqrt(vp_kPa)) * ((tmax[[i]]^4 + tmin[[i]]^4) / 2)

  Rnl_wm2 = Rnl_mj / .0864 #Convert back to Wm2

  radW2 = srad[[i]] + Rnl_wm2

  rad = (radW2 * daylength / 1000000) * 277.78

  # PET -------------------------------------------------

  VPD = es - (vp_kPa * 100)

  DEL = (4009*es) / ((tair - 273.15)*(tair - 273.15))

  GAM = .000646 * (1 + .000946 * (tair- 273.15)) * (pressure_Pa / 1000)

  W = DEL / (DEL + GAM)

  FU2 = 0.030 + 0.0576 * wind[[i]]

  NR = rad / (694.5 * (1-.000946 * (tair- 273.15)))

  PET.list[[i]] = W * NR + ( (1-W)* VPD * FU2 )

  file = names(tmax[[i]])
  write.folder = paste0(core, xx$scenario[1], "/")
  raster::writeRaster(tmax[[i]], filename=paste0(write.folder,"/tasmax/", xx$model[1], "tasmax_", gsub("X", "", file), ".tif"), format="GTiff", overwrite=TRUE, options=c('TFW=YES', "COMPRESS=LZW"))
  raster::writeRaster(tmin[[i]], filename=paste0(write.folder,"/tasmin/", xx$model[1], "tasmin_", gsub("X", "", file), ".tif"), format="GTiff", overwrite=TRUE, options=c('TFW=YES', "COMPRESS=LZW"))
  raster::writeRaster(wind[[i]], filename=paste0(write.folder,"wind/", xx$model[1], "wind_", gsub("X", "", file), ".tif"), format="GTiff", overwrite=TRUE, options=c('TFW=YES', "COMPRESS=LZW"))
  raster::writeRaster(shum[[i]], filename=paste0(write.folder,"shum/", xx$model[1], "shum_", gsub("X", "", file), ".tif"), format="GTiff", overwrite=TRUE, options=c('TFW=YES', "COMPRESS=LZW"))
  raster::writeRaster(srad[[i]], filename=paste0(write.folder,"dwrad/", xx$model[1], "dwrad_", gsub("X", "", file), ".tif"), format="GTiff", overwrite=TRUE, options=c('TFW=YES', "COMPRESS=LZW"))
  raster::writeRaster(rad, filename=paste0(write.folder,"totrad/", xx$model[1], "totrad_", gsub("X", "", file), ".tif"), format="GTiff", overwrite=TRUE, options=c('TFW=YES', "COMPRESS=LZW"))
  raster::writeRaster(rh, filename=paste0(write.folder,"rhum/", xx$model[1], "rhum_", gsub("X", "", file), ".tif"), format="GTiff", overwrite=TRUE, options=c('TFW=YES', "COMPRESS=LZW"))
  raster::writeRaster(vp_kPa, filename=paste0(write.folder,"vp/", xx$model[1], "vp_", gsub("X", "", file), ".tif"), format="GTiff", overwrite=TRUE, options=c('TFW=YES', "COMPRESS=LZW"))
  raster::writeRaster(PET.list[[i]], filename=paste0(write.folder,"pet/", xx$model[1], "pet_", gsub("X", "", file), ".tif"), format="GTiff", overwrite=TRUE, options=c('TFW=YES', "COMPRESS=LZW"))

}

}





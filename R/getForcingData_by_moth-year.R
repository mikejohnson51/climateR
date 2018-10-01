library(geoknife)
library(AOI)

## DEFINE STENCIL
stencil = sf::read_sf("/Users/mikejohnson/Documents/WRF_Hydro/Shape/WBDHU2.shp") %>%
  getBoundingBox(sf = T) %>%
  sf::st_buffer(.15) %>%
  sf::as_Spatial() %>% sp::spTransform('+proj=longlat +datum=WGS84') %>%
  simplegeom()


write_path = '/Volumes/Seagate5tb/FORCINGS_MACA'
#'/Users/mikejohnson/Documents/FORCING'

knife = webprocess(algorithm = list('OPeNDAP Subset'="gov.usgs.cida.gdp.wps.algorithm.FeatureCoverageOPeNDAPIntersectionAlgorithm"))

param = c('tasmax', 'tasmin','pr','rsds','uas','vas','huss')
model =   'HadGEM2-CC365' #"GFDL-ESM2M"
year = 2050

# RUN
system.time({

#for(y in 1:length(years)){
#year = years[y]

dates = seq(as.POSIXct(paste0(year,"-01-01 00:00:00")), as.POSIXct(paste0(year,"-12-31 00:00:00")), 60*60*24)

for(j in 1:2){

options = c("rcp45", 'rcp85')

scenario = options[j]

fabric <- webdata(url = "dods://cida.usgs.gov/thredds/dodsC/macav2metdata_daily_future",
                  variables = c(paste(param, model, "r1i1p1", scenario, sep = "_")))

for(i in 8:12){

d = dates[as.numeric(substr(dates, 6,7))  == i]
times(fabric) <- c(head(d, 1), tail(d, 1))

job <- geoknife(stencil, fabric, knife, wait = TRUE, OUTPUT_TYPE="netcdf")

cat(crayon::cyan(geoknife::check(job)$status, "Month:"), i, "\n")

dir = paste0(write_path, "/", model, "/", scenario, "/", year)
if(!dir.exists(dir)){dir.create(dir, recursive = T)}

path = paste0(dir, "/", substr(times(fabric)[1],1,4),"_", sprintf("%02d", i), "_macaV2metdata.nc")

download(job, destination = path, overwrite = T)
cat(crayon::yellow("Downloaded Month:"), i, "\n")

system(paste("cdo splitsel,1", path, paste0(dirname(path),"/tmp")))

cat(crayon::green("Data Split Month:"), i, "\n")

file.rename(list.files(dir, pattern = 'tmp', full.names = T), paste0(dir, "/", as.Date(d),"_macaV2metdata.nc"))

file.remove(path)

cat(crayon::red("Finished Month:"), i, scenario, "\n")
}
cat(crayon::white("Finished Scenario:"), scenario, "\n")
cat(crayon::white("Finished Scenario:"), scenario, "\n")
cat(crayon::white("Finished Scenario:"), scenario, "\n")
}
cat(crayon::white("Finished year:"), year, "\n")
cat(crayon::white("Finished year:"), year, "\n")
cat(crayon::white("Finished year:"), year, "\n")
#}
})



print(length(list.files(paste0(write_path, "/", model, "/rcp45"), recursive = TRUE)) == length(list.files(paste0(write_path, "/", model, "/rcp85"), recursive = T)))


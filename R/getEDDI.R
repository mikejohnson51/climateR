#Data are EDDI values for the period prior to the indicated date that is equal to the timescale.

getEDDI =  function(AOI = NULL, startDate, endDate = NULL,  timestep = "3 week") {

  d = define.dates(startDate, endDate = endDate)
  tmp = unlist(strsplit(timestep, " "))
  num = sprintf("%02s", tmp[1])
  abb = ifelse(tmp[2] == 'week', 'wk', 'mn')
  base = "ftp://ftp.cdc.noaa.gov/Projects/EDDI/CONUS_archive/data/"

  urls <- paste0(base, d$year, "/", "EDDI_ETrs_", num, abb, "_", d$string, ".asc")

  s = raster::stack()

  for (i in seq_along(urls)) {
      dest = tempfile(pattern = basename(url[i]))
      suppressWarnings( httr::GET(urls[i], httr::write_disk(dest, overwrite = TRUE)) ) # suppressing Government Warning
      r = raster::raster(dest)
      raster::crs(r) = "+init=epsg:4326"
      r = raster::crop(r, AOI, snap = "out")
      s = raster::addLayer(s, r)
  }

  names(s) = gsub(".asc", "", basename(url))
  return(s)
}

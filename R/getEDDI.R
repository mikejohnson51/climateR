#' @title Get EDDI Climate Data for an Area of Interest
#' @description The EDDI (Evaporative Demand Drought Index) is an experimental drought monitoring and early warning guidance tool. 
#' It examines how anomalous the atmospheric evaporative demand (E0; also known as "the thirst of the atmosphere") is for a given location 
#' across a time period of interest. EDDI is multi-scalar, meaning that this time  period —can vary to capture drying dynamics 
#' that themselves operate at different timescales. The EDDI for a given date is the aggregated values for a time period before hand. 
#' The time period is given in either a number of months or number of weeks. For example, a timestep of 3 and a timescale of weeks requests 
#' the EDDI values for the 3 weeks prior to the requested date.
#' @param AOI a spatial polygon object (sf or sp)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @param timestep  The number of time steps (numeric). Must be between 1-12.
#' @param timescale Can be 'week' or 'month'
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export

getEDDI =  function(AOI = NULL, startDate, endDate = NULL,  timestep = 3, timescale = "week") {
  
  if(!timescale %in% c("week", 'month')){
    stop("Timescale must be either week or month")
  }
  
  if(timestep > 12 | timestep < 0){
    stop("Timestep must be between 0 and 12")
  }

  g = define.grid(AOI, 'eddi')
  d = define.dates(startDate, endDate = endDate)
  num = sprintf("%02s", timestep)
  abb = ifelse(timescale == 'week', 'wk', 'mn')

  urls <- paste0(g$base, d$year, "/", "EDDI_ETrs_", num, abb, "_", d$string, ".asc")

  system.time({
  s = raster::stack()

  for (i in seq_along(urls)) {
      dest = tempfile(pattern = basename(urls[i]))
      suppressWarnings( httr::GET(urls[i], httr::write_disk(dest, overwrite = TRUE)) ) # suppressing Government Warning
      r = raster::raster(dest)
      raster::crs(r) = g$proj
      r = raster::crop(r, AOI, snap = "out")
      s = raster::addLayer(s, r)
  }

  })
  
  names(s) = gsub(".asc", "", basename(urls))
  
  s
}

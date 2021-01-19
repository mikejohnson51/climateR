#' @title Get CHIRPS Precipitation Data for an Area of Interest
#' @description Climate Hazards Group InfraRed Precipitation with Station data (CHIRPS) is a 30+ year quasi-global rainfall dataset. Spanning 50°S-50°N (and all longitudes), starting in 1981 to near-present,
#' CHIRPS incorporates 0.05° resolution satellite imagery with in-situ station data to create gridded rainfall time series for trend analysis and seasonal drought monitoring.
#' @param AOI a spatial polygon object (sf or sp)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return a list of rasterStacks or dataframe of point values
#' @export

getCHIRPS = function(AOI, startDate, endDate = NULL  ){

  i = NULL
  d = define.dates(startDate, endDate)
  dates = paste0("%28", as.numeric(format(d$date, "%d")), "%20", 
                 month.abb[as.numeric(format(d$date, "%m"))], "%20", 
                 format(d$date, "%Y"))
  
  g = define.grid(AOI, 'chirps')

  if(methods::is(AOI, 'bbox')){
    bb = AOI
  } else {
    bb =  sf::st_bbox(AOI)
  }

  urls = paste0(g$base,
         'Y/', bb$ymin, '/', bb$ymax,'/RANGEEDGES/',
         'X/',bb$xmin, '/', bb$xmax,'/RANGEEDGES/',
         'T/',dates,'%29VALUES/%5BX/Y/%5D/palettecolor.tiff?filename=tmp.tiff')

 `%dopar%` <- foreach::`%dopar%`
  no_cores  <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  if(g$type != "grid") {

    var = foreach::foreach(i = 1:length(urls), .combine = 'c', .packages = 'raster') %dopar% { 
      raster::raster(urls[i]) %>% as.matrix()
    }

    l = length(d$date)
    df = data.frame(source = rep('chirps', l), lat = rep(bb$ymax , l), lon = rep(bb$xmax, l),  date = d$date, stringsAsFactors = FALSE)

    mat = matrix(var, nrow = l, byrow = F)
    s = cbind(df, mat)
    colnames(s) = c('source', 'lat', 'lon', 'date', 'prcp')

  } else {
    s = foreach::foreach(i = 1:length(urls), .combine = 'stack', .packages = 'raster') %dopar% { raster::raster(urls[i])}
    names(s) = paste0("prcp_", d$string)
  }

  s
}


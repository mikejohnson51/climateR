.orient = function(mat, fun = NULL) {
  if (fun == 't') {
    tmp = t(mat)
  } else {
    tmp  = apply(t(mat), 2, rev)
  }
  return(tmp)
}

#' @title Fast Download for OPenDAP URLs
#' @description Parrallelized download and processing of TDS data
#' @param urls a set of OPeNDAP URLS
#' @param params a set of parameters
#' @param names a set of variables names
#' @param g  a list of grid data (found with define.grid)
#' @param date.names a list of parsed dates
#' @param dataset a dataset name (source/id)
#' @param fun a function to structure TDS output
#' @param no_data a no_date value
#' @param scale_factor a scale factors for returned data
#' @return a list object of raster*
#' @export


fast.download = function(urls, params, names, g, date.names, dataset, fun = 'r', no_data = NA, scale_factor = 1){

  stopifnot(identical(length(urls), NROW(params)))

  `%dopar%` <- foreach::`%dopar%`
  no_cores  <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  if(g$type == 'point'){
    var = foreach::foreach(i = 1:length(urls), .combine = 'c') %dopar% { 
      tryCatch({
        RNetCDF::open.nc(urls[i]) %>% RNetCDF::var.get.nc(params[i], unpack = T) 
      }, error = function(e){
        urls[i]
      })
    }
  } else {
    var = foreach::foreach(i = 1:length(urls)) %dopar% { 
      tryCatch({
        RNetCDF::open.nc(urls[i]) %>% RNetCDF::var.get.nc(params[i], unpack = T) 
      }, error = function(e){
          urls[i]
      })
    }
  }

  if(g$type == 'grid'){

    b = list()

    for(i in 1:length(var)){

    v = var[[i]] * scale_factor
    
    if(!is.na(no_data)){ 
    if(no_data > 0){
      v[v > no_data] = NA
    } else {
      v[v < no_data] = NA
    }
        
    }
    
    time = as.integer((length(v) / (g$rows * g$cols)))
    
    if(time > 0){
      dim(v) = ceiling(c(g$rows, g$cols, time))
      var2 = array(rep(0, length(v)), dim = ceiling(c(g$rows, g$cols, time)))
      for(j in 1:dim(var2)[3]){ var2[,,j] = .orient(v[,,j], fun = fun) }
      b1 = raster::brick(var2)
    } else { 
      mat = .orient(v, fun = fun)
      b1 = raster(mat)
    }
    
    raster::extent(b1) = g$e
    raster::crs(b1) = g$proj


    b1[b1>100000] = NA
    b[[i]] = b1
  }

    names(b) = names

    keys = unique(names(b))

    b = sapply(keys, function(name) {stack(b[grep(paste0(name, "$"), names(b))])})

    for(i in 1:length(b)){ names(b[[i]]) = unique(date.names[[i]])}

  } else {

    l = length(date.names)
    df = data.frame(source = rep(dataset, l), lat = rep(g$lat, l), lon = rep(g$lon, l),  date = date.names, stringsAsFactors = FALSE)

    mat = matrix(var, nrow = l, byrow = F)
    b = cbind(df, mat)
    colnames(b) = c('source', 'lat', 'lon', 'date', unique(names))
  }

return(b)

}

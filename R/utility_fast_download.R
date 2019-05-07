.orient = function(mat, fun = NULL) {
  if (fun == 't') {
    tmp = t(mat)
  } else {
    tmp  = apply(t(mat), 2, rev)
  }
  return(tmp)
}


fast.download = function(urls, params, names, g, date.names, dataset, fun = 'r'){

  stopifnot(identical(length(urls), NROW(params)))

  `%dopar%` <- foreach::`%dopar%`
  no_cores  <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  if(g$type == 'point'){
    var = foreach::foreach(i = 1:length(urls), .combine = 'c') %dopar% { RNetCDF::open.nc(urls[i]) %>% RNetCDF::var.get.nc(params[i], unpack = T)}
  } else {
    var = foreach::foreach(i = 1:length(urls)) %dopar% { RNetCDF::open.nc(urls[i]) %>% RNetCDF::var.get.nc(params[i], unpack = T) }
  }

  if(g$type == 'grid'){

    b = list()

    for(i in 1:length(var)){

    v = var[[i]]

    if(length(dim(v)[3]) == 0 | is.na(dim(v)[3])){
      var2 = .orient(v, fun = fun)
      b1 = raster::raster(var2)
      raster::crs(b1) = g$proj
      raster::extent(b1) = g$e
    } else {
      var2 = array(0, dim = c(dim(v)[2], dim(v)[1],dim(v)[3]))
      for(j in 1:dim(v)[3]){ var2[,,j] = .orient(v[,,j], fun = fun) }
      b1 = raster::brick(var2)
      raster::crs(b1) = g$proj
      raster::extent(b1) = g$e
    }

    b1[b1>100000] = NA
    b[[i]] = b1
    }

    names(b) = names

    keys = unique(names(b))

    b = sapply(keys, function(name) {stack(b[grep(name, names(b))])})

   for(i in 1:length(b)){ names(b[[i]]) = unique(date.names)}

  } else {

    l = length(date.names)
    df = data.frame(source = rep(dataset, l), lat = rep(g$lat, l), lon = rep(g$lon, l),  date = date.names, stringsAsFactors = FALSE)

    mat = matrix(var, nrow = l, byrow = F)
    b = cbind(df, mat)
    colnames(b) = c('source', 'lat', 'lon', 'date', unique(names))
  }

return(b)

}

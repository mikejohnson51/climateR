getRaster = function(r){
xx = names(r$var)
nc = r

no_cores <- parallel::detectCores() - 1
doParallel::registerDoParallel(no_cores)

res <-
  foreach::foreach(i = 1:length(xx) , .combine = raster::stack) %dopar% {

    vals = ncdf4::ncvar_get(nc, xx[i])

    mat <- apply(t(vals),2,rev)
    r <-raster::raster(mat)
    raster::projection(r) = sp::CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs")
    raster::extent(r) = c(lon[min.lon], lon[max.lon], lat[min.lat], lat[max.lat])
    names(r) = xx[i]

    return(r)

  }

}


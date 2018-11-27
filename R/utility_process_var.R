.convert.to.raster = function(mat, fun) {
  if (fun == 't') {
    tmp = raster::raster(t(mat))
  } else {
    tmp  = raster::raster(apply(t(mat), 2, rev))
  }
  return(tmp)
}

process.var = function(group, g, var, dates, param, name = NULL, fun = 'r', proj = '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs'){

  if(g$type == 'point'){
    group[[param]][[name]] = var
  } else {

  ncol   = abs(g$xmax - g$xmin)  + 1
  nrow   = abs(g$ymax - g$ymin ) + 1
  if(length(dim(var)) == 3){ var.l = lapply(seq(dim(var)[3]), function(x) var[ , , x]) } else { var.l = list(var)}
  r = list()

  if(g$type == 'point'){

    if(all(is.na(var))){cat("Nearest cell centroid contains NA values. Probably lies in the ocean.")}
    group[[param]] = var

  } else{

    for(j in 1:length(var.l)){
      m = matrix(var.l[[j]], ncol = nrow, nrow = ncol, byrow = FALSE)
      r[[paste0('X', dates[j])]] = .convert.to.raster(var.l[[j]], fun)
    }

    r = raster::stack(r)
    raster::extent(r) <- g$e
    raster::crs(r)    <- proj
    group[[paste0(param,"_", name)]]    = r

  }
  }

  return(group)

}


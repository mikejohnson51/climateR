#' @title Extract Sites
#' @description extract timeseries values from a raster stack for a set of points
#' @param r a SpatRaster object
#' @param pts point to extract from
#' @param ID the unique identifier of each point (column name from pts)
#' @return a data.frame with columes representing points, and rows time periods
#' @family dap
#' @export


extract_sites = function(r, pts, ID = NULL){
  
  if(!is.list(r)){
    r = list(r)
  }
  
  pts = spatAOI(pts)
  
  if(is.null(ID)){
    tmp = sapply(1:ncol(pts), FUN = function(x){ length(terra::unique(pts[,x])) }) 
    tmp = tmp == nrow(pts)
    if(all(!tmp)){
      warning("No unique POINT identifiers. Returning gridded data")
      return(r)
    } else {
      ID = names(pts)[which.min(tmp)]
    }
  } else {
    if((nrow(unique(pts[[ID]])) != nrow(pts))){
      warning("No unique POINT identifiers. Returning gridded data")
      return(r)
    }
  }

  flip = function(x, r, pts, ID){
    df = data.frame(t(extract(r[[x]], pts, ID = FALSE)), row.names = NULL)
    names(df) <- paste0(gsub(" ", "",  pts[[ID]][,1]))
    df = mutate_all(df, as.numeric)
    df$date = terra::time(r[[x]])
    select(df, date, everything())
  }
  
  pts = project(pts, crs(r[[1]]))
  # Extract your climate data via the points
  extdata = lapply(1:length(r), function(x){ flip(x, r, pts, ID) } )
  
  if(length(r) == 1){
    extdata = extdata[[1]]
  } else {
    names(extdata) = names(r)
  }

  extdata
}

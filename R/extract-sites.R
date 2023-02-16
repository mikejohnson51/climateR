#' @title Extract Sites
#' @description extract timeseries values from a raster stack for a set of points
#' @param r a SpatRaster object
#' @param pts point to extract from
#' @param id the unique identifier of each point (column name from pts)
#' @return a data.frame with columes representing points, and rows time periods
#' @export


extract_sites = function(r, pts, id){
  
  if(inherits(pts, "sf")){
    pts = vect(pts)
  }

  flip = function(x, r, pts, id){
    df = data.frame(t(extract(r[[x]], pts, ID = FALSE)), row.names = NULL)
    names(df) <- paste0(gsub(" ", "",  pts[[id]][,1]))
    df = mutate_all(df, as.numeric)
    df$date = terra::time(r[[x]])
    select(df, date, everything())
  }
  
  pts = project(pts, crs(r[[1]]))
  # Extract your climate data via the points
  extdata = lapply(1:length(r), function(x){ flip(x, r, pts, id) } )
  
  if(length(r) == 1){
    extdata[[1]]
  } else {
    names(extdata) = names(r)
  }

  extdata
}

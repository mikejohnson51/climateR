#' @title Extract Sites
#' @description extract timeseries values from a raster stack for a set of points
#' @param r a raster object
#' @param pts point to extract from
#' @param id the unique identifier of each point (column name from pts)
#' @return a data.frame with columes representing points, and rows time periods
#' @export


extract_sites = function(r, pts, id){
  
  if(inherits(pts, "sf")){
    pts = vect(pts)
  }
  
  my.to.date = function(x){
    d = gsub("[.]", "-",gsub("X", "", x))
    #if(nchar(d[1]) == 7){d = paste0(d, "-01")}
    
    if(nchar(d[1]) !=10){
      as.POSIXct(d, tz = "UTC")
    } else {
      as.Date(d)
    }
  }
  
  flip = function(x, r, pts, id){
    df = data.frame(t(extract(r[[x]], pts, ID = FALSE)), row.names = NULL)
    names(df) <- paste0("site_", gsub(" ", "",  pts[[id]][,1]))
    df = mutate_all(df, as.numeric)
    df$date = my.to.date(names(r[[x]]))
    select(df, date, everything())
  }
  
  pts = project(pts, crs(r[[1]]))
  # Extract your climate data via the points
  extdata = lapply(1:length(r), function(x){ flip(x, r, pts, id) } )
  
  names(extdata) = names(r)
  
  extdata
}

#' @title Extract Sites
#' @description extract timeseries values from a raster stack for a set of points
#' @param r a raster object
#' @param pts point to extract from
#' @param id the unique identifier of each point (column name from pts)
#' @return a data.frame with columnes represneting points, and rows time periods
#' @export

extract_sites = function(r, pts, id){
  
  my.to.date = function(x){
    d = gsub("[.]", "-",gsub("X", "", x))
    if(nchar(d[1]) == 7){d = paste0(d, "-01")}
    as.Date(d)
  }
  
  flip = function(x, r, pts, id){
    df = cbind(ID = pts[[id]], raster::extract(r[[x]], pts))
    df = data.frame(t(df))
    names(df) <- paste0("site_", df[1,])
    df = df[-1,]
    df = cbind(date = my.to.date(grep("X", rownames(df), value = TRUE)), df)
    rownames(df) = NULL
    df
  }
  
  
  pts = st_transform(pts, crs(r[[1]]))
  # Extract your climate data via the points
  extdata = lapply(seq_along(r), function(x){ flip(x, r, pts, id) } )
  
  x = 1
  
  
  names(extdata) = names(r)
  
  extdata
}

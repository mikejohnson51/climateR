#' @title Extract Sites
#' @description extract timeseries values from a raster stack for a set of points
#' @param r a raster object
#' @param pts point to extract from
#' @param id the unique identifier of each point (column name from pts)
#' @return a data.frame with columnes represneting points, and rows time periods
#' @export
#' @importFrom raster extract

extract_sites = function(r, pts, id){
  
  my.to.date = function(x){
    d = gsub("[.]", "-",gsub("X", "", x))
    if(nchar(d[1]) == 7){d = paste0(d, "-01")}
    
    if(nchar(d[1]) !=10){
      d
    } else {
      as.Date(d)
    }

  }
  
  flip = function(x, r, pts, id){
    df = cbind(ID = pts[[id]], raster::extract(r[[x]], pts))
    df = data.frame(t(df))
    names(df) <- paste0("site_", df[1,])
    df = df[-1,]
    df = cbind(date = my.to.date(names(r[[x]])), df)
    rownames(df) = NULL
    df
  }
  
  
  pts = sf::st_transform(pts, sf::st_crs(r[[1]]))
  # Extract your climate data via the points
  extdata = lapply(1:length(r), function(x){ flip(x, r, pts, id) } )
  
  names(extdata) = names(r)
  
  extdata
}

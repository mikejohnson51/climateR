#' @title Make Grid
#' @description This function creates a simple feature layer from an input raster to be used with buildWeights and zonal
#' @param r a raster object
#' @return a sf layer of polygons
#' @export
#' @examples
#' \dontrun{
#' AOI = getAOI(state = "CA", county = "all")
#'
#' r = getMACA(AOI, "prcp", startDate = "2018-10-10")
#' g = makeGrid(r)
#' }
#' @author Mike Johnson

makeGrid = function(r){

  grd_lrg <- spex::qm_rasterToPolygons(r, na.rm = F)

  y.dim = dim(r)[1]
  x.dim = dim(r)[2]

  grd_lrg['grid_ids'] <- as.vector(t(matrix(c(1:(x.dim*y.dim)),
                                            nrow = y.dim,
                                            ncol = x.dim,
                                            byrow = F)))

  grd_lrg['totalArea'] = sf::st_area(grd_lrg[1,])

  return(grd_lrg)
}

#' @title Build Area Weighted Intersection Data Frame
#' @description Returns the fractional percent of each grid feature covered by each intersecting polygon feature.
#' These can be used as the weights in an area-weighted mean overlay analysis zonal
#' @param grid a sf data.frame
#' @param poly a sf data.frame
#' @param abb The feature (poly) attribute to intersect by
#' @return a sf layer of polygons
#' @export
#' @examples
#' \dontrun{
#' AOI = getAOI(state = "CA", county = "all")
#'
#' r = getMACA(AOI, "prcp", startDate = "2018-10-10")
#' g = makeGrid(r)
#' w = buildWeights(g, AOI, "countyfp")
#' }
#' @author Mike Johnson

buildWeights = function(grid, poly, abb = NULL){

  if(is.null(abb)){ abb = names(poly)[1]}
  if(!AOI::checkClass(poly, "sf")) { poly = sf::st_as_sf(poly) }
  if(sf::st_crs(poly) != sf::st_crs(grid)) { poly  <- sf::st_transform(poly, sf::st_crs(grid)) }
  if(abb %in% names(poly)){ poly = poly[ ,abb] } else { stop(paste(abb, "is not an attribute of polygon input"))}

  return(suppressMessages(suppressWarnings(sf::st_intersection(grid, poly) %>%
                                             dplyr::mutate(cellarea = sf::st_area(.),
                                                    areaWeight = as.numeric(cellarea / totalArea)) %>%
                                             sf::st_drop_geometry() %>%
                                             dplyr::right_join(grid, by = 'grid_ids') %>%
                                             dplyr::select(grid_ids, !!abb, areaWeight))))
}

#' @title Compute Zonal Averaging
#' @description Returns a simple features layer with average values by polygon featrue
#' @param grid a sf data.frame
#' @param poly a sf data.frame
#' @param w a weighted intersection data.frame
#' @return a sf layer of polygons
#' @export
#' @examples
#' \dontrun{
#' AOI = getAOI(state = "CA", county = "all")
#'
#' r = getMACA(AOI, "prcp", startDate = "2018-10-10")
#' g = makeGrid(r)
#' w = buildWeights(g, AOI, "countyfp")
#' }
#' @author Mike Johnson



zonal = function(grid, poly, w){

  abb = names(w)[2]

  names(w)[1:3] = c("grid_ids", "poly_id", "w")

  if(!AOI::checkClass(poly, "sf")) { poly = sf::st_as_sf(poly) }

  w = w %>% right_join(st_drop_geometry(grid), by = "grid_ids")

  s = split(w[ , !(names(w) %in% c("poly_id", "grid_ids"))], w$poly_id) %>%
    lapply(function(x) (colSums(x * x$w, na.rm = T) / sum(x$w))   )

  df = data.frame(poly_id = names(s), do.call(rbind, s), stringsAsFactors = F)

  names(df) = c("poly_id", "w", names(w)[4:NCOL(w)])

  return( full_join(poly, df, by = setNames('poly_id', abb)))

}

assign_grid = function(grid, new){

  index = which(!names(grid) %in% c("geometry", "grid_ids","totalArea", names(r)))
  grid[,names(new)] = raster::getValues(new)

  if(length(index) > 0){
    grid = grid[,-index]
  }

  return(grid)

}




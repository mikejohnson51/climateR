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

buildWeights = function(grid, poly, abb = NULL){

  if(is.null(abb)){ abb = names(poly)[1]}
  if(!AOI::checkClass(poly, "sf")) { poly = sf::st_as_sf(poly) }
  if(sf::st_crs(poly) != sf::st_crs(grid)) { poly  <- sf::st_transform(poly, sf::st_crs(grid)) }
  if(abb %in% names(poly)){ poly = poly[ ,abb] } else { stop(paste(abb, "is not an attribute of polygon input"))}

  return(suppressMessages(suppressWarnings(sf::st_intersection(grid, poly) %>%
                                             mutate(cellarea = sf::st_area(.),
                                                    areaWeight = as.numeric(cellarea / totalArea)) %>%
                                             sf::st_drop_geometry() %>%
                                             right_join(grid, by = 'grid_ids') %>%
                                             select(grid_ids, !!abb, areaWeight))))
}

zonal = function(grid, poly, w){

  abb = names(w)[2]

  names(w)[1:3] = c("grid_ids", "poly_id", "w")

  if(!AOI::checkClass(poly, "sf")) { poly = sf::st_as_sf(poly) }

  w = w %>% right_join(st_drop_geometry(grid), by = "grid_ids")

  s = split(w[ , !(names(w) %in% c("poly_id", "grid_ids"))], w$poly_id) %>%
    lapply(function(x) (colSums(x * x$w, na.rm = T) / sum(x$w))   )

  df = data.frame(poly_id = names(s), do.call(rbind, s), stringsAsFactors = F)

  names(df) = c("poly_id", "w", names(w3)[4:NCOL(w)])

  return( full_join(poly, df, by = setNames('poly_id', abb)))

}

assign_grid = function(grid, new){

  index = which(!names(grid) %in% c("geometry", "grid_ids","totalArea"))

  grid[, index] = raster::getValues(new)

  return(grid)

}


AOI  = getAOI(state = "conus")

sp::plot(AOI)

p = getMACA(AOI, "tmax", startDate = "2018-10-10")

r = p$tmax_rcp45_CCSM4

raster::plot(r)

system.time({g = makeGrid(r)})


AOI = getAOI(state = c('CA', "UT", 'NV', 'AZ'), county = 'all')

r = getPRISM(AOI, param = "prcp", startDate = '2018-10-10')
r = r$prcp

system.time({
  g  = make_grid2(r) %>% select(-totalArea)
  ww = get_weights(g, AOI, abb = 'countyfp')
  z  = zonal_stats1(g, AOI, ww) ## 34.926
})

system.time({
  grid = make_grid2(r)
  w = buildWeights(grid, AOI, abb = 'countyfp')
  z2 = zonal_stats1(grid, AOI, w) ## 24.221
})

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

make_grid2 = function(r){

  grd_lrg <- spex::polygonize(r, na.rm = F)

  y.dim = dim(r)[1]
  x.dim = dim(r)[2]

  grd_lrg['grid_ids'] <- as.vector(t(matrix(c(1:(x.dim*y.dim)),
                                            nrow = y.dim,
                                            ncol = x.dim,
                                            byrow = F)))

  grd_lrg['totalArea'] = sf::st_area(grd_lrg)

  return(grd_lrg)
}

get_weights = function(grid, poly, abb = NULL){

  if(is.null(abb)){ abb = names(poly)[1]}

  if(!AOI::checkClass(poly, "sf")) { poly = sf::st_as_sf(poly) }

  if(st_crs(poly) != st_crs(grid)) { poly  <- st_transform(poly, st_crs(grid)) }

  if(abb %in% names(poly)){ poly = poly[,abb] } else { stop(paste(abb, "is not an attribute of polygon input"))}

  suppressMessages (

  int <-  areal::aw_intersect(poly,
                              source  = grid,
                              areaVar = "area") %>%

          areal::aw_total(source = grid,
                        id       = "grid_ids",
                        areaVar  = "area",
                        totalVar = "totalArea",
                        type     = "extensive",
                        weight   = "total") %>%

          areal::aw_weight(areaVar   = "area",
                         totalVar    = "totalArea",
                          areaWeight = "areaWeight") %>%

          data.frame()
  )

  return( merge(int[, names(int) %in% c('grid_ids', abb, 'areaWeight')],
                st_set_geometry(grid[,"grid_ids"],  NULL),
                by = "grid_ids",
                all.y = T) )
}

zonal_stats1 = function(grid, poly, w){

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

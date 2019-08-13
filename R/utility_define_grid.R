#' @title Define AOI native grid
#' @description **INTERNAL** This function defines the grid of the AOI in terms of the climate resource
#' @keywords internal
#' @param AOI an AOI object
#' @param source a datset id (source)  being called
#' @return a list of values including type, lat.call, lon.call, extent, AOI, proj4string
#' @keywords internal
#' @author Mike Johnson


define.grid3 = function(AOI, source = NULL){

  index = which(grid_meta$source == source)

  if(AOI::checkClass(AOI, 'sp')){ AOI = sf::st_as_sf(AOI) }
  if(checkClass(AOI, 'data.frame') & !checkClass(AOI, "sf")){  AOI =  sf::st_sfc(list(sf::st_point(c(AOI$lon, AOI$lat)))) %>% sf::st_set_crs(AOI::aoiProj) %>% sf::st_sf() }

  bb = AOI %>% sf::st_transform(grid_meta$proj[index]) #%>% AOI::bbox_st()
  
  domain = suppressWarnings( AOI::bbox_sp(c(grid_meta$MinX[index], grid_meta$MaxX[index], grid_meta$MinY[index], grid_meta$MaxY[index])) %>% 
     sf::st_set_crs(grid_meta$proj[index]) )
 
  if(!AOI::is_inside(domain, bb, total = T)){ 
    
    if(AOI::is_inside(domain, bb, total = F)){
      message("Requested AOI not completly in model domain...AOI is being clipped") 
      bb = sf::st_intersection(domain, bb)
    } else {
      stop("Requested AOI not in model domain")
    }
  }

  bb = AOI::bbox_st(bb)
  
  X_coords <- seq(as.numeric(grid_meta$MinX[index]),
                  as.numeric(grid_meta$MaxX[index]),
                  by = as.numeric(grid_meta$dX[index]))

  Y_coords <- seq(as.numeric(grid_meta$MinY[index]),
                  as.numeric(grid_meta$MaxY[index]),
                  by = as.numeric(grid_meta$dY[index]))
  


  if(any(sf::st_geometry_type(AOI) != 'POINT')) {
    
    g = list(
      ymax = max(which.min(abs(Y_coords - bb$ymax)), which.min(abs(Y_coords - bb$ymin))) ,
      ymin = min(which.min(abs(Y_coords - bb$ymax)), which.min(abs(Y_coords  - bb$ymin))) ,
      xmax = max(which.min(abs(X_coords - bb$xmin)),  which.min(abs(X_coords - bb$xmax))),
      xmin = min(which.min(abs(X_coords - bb$xmin)),  which.min(abs(X_coords - bb$xmax)))
    )
    

    g[['type']] = 'grid'
    g[['lat.call']] = paste0('[', g$ymin - 1 , ':1:', g$ymax - 1, ']')
    g[['lon.call']] = paste0('[', g$xmin - 1,  ':1:', g$xmax - 1, ']')
    g[['e']] = raster::extent(range(X_coords[c(g$xmin:g$xmax)]), range(Y_coords[c(g$ymin:g$ymax)]))
    g[['AOI']] = AOI
    g[['proj']] = grid_meta$proj[index]

  } else {

    lon = bb$xmax
    lat = bb$ymax

    if(sum(c( lon > min(X_coords), lon < max(X_coords), lat > min(Y_coords), lat < max(Y_coords))) != 4) {
      stop('Requested Point not within domain')
    }

    g = list(
      y = which.min(abs(Y_coords  - lat)),
      x = which.min(abs(X_coords  - lon))
    )

    g[["lat"]]      = Y_coords[g$y]
    g[["lon"]]      = X_coords[g$x]
    g[['type']]     = 'point'
    g[['lat.call']] = paste0('[', g$y - 1 , ':1:', g$y - 1, ']')
    g[['lon.call']] = paste0('[', g$x - 1,  ':1:', g$x - 1, ']')
    g[['e']] = NULL
    g[['AOI']] = AOI
    g[['proj']] = grid_meta$proj[index]

  }

  return(g)
}


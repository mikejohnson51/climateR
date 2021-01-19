#' @title Define AOI native grid
#' @description **INTERNAL** This function defines the grid of the AOI in terms of the climate resource
#' @keywords internal
#' @param AOI an POINT or POLYGON object
#' @param source a dataset id (source)  being called
#' @return a list of values including type, lat.call, lon.call, extent, AOI, proj4string
#' @keywords internal
#' @importFrom methods is
#' @importFrom sf st_geometry_type st_as_sf st_as_sfc st_bbox st_transform st_combine st_crs st_within st_intersects st_intersection
#' @importFrom  raster extent

define.grid = function(AOI, source = NULL){
  
  grid = climateR::grid_meta[which(climateR::grid_meta$source == source), ]
  
  if(methods::is(AOI, 'bbox')){ 
    AOI = sf::st_as_sfc(AOI) 
  }
  
  if(methods::is(AOI, 'sp')){ 
    AOI = sf::st_as_sf(AOI) 
  }
  
  if(any(sf::st_geometry_type(AOI) == "POINT" & nrow(AOI) > 1, is.null(nrow(AOI)))){ 
    AOI = sf::st_as_sfc(sf::st_bbox(AOI)) 
  }
  
  bb = sf::st_combine(sf::st_transform(AOI, grid$proj))
  
  X_coords <- seq(grid$Xstart, grid$Xend, by = grid$dX)
  if(any(X_coords > 180.001)){X_coords = X_coords - 360}
  
  Y_coords <- seq(grid$Ystart, grid$Yend, by = grid$dY)
  
  domain = sf::st_as_sfc(sf::st_bbox(c(xmin = min(X_coords), xmax = max(X_coords), 
                                       ymin = min(Y_coords), ymax = max(Y_coords)), 
                                       crs = sf::st_crs(bb)))
  
  suppressMessages(suppressWarnings(
    if(!sf::st_within(bb, domain, sparse = FALSE)){ 
      if(sf::st_intersects(bb, domain, sparse = FALSE)){
        message("Requested AOI not completly in model domain ... AOI is being clipped") 
        bb = sf::st_intersection(domain, bb)
      } else {
        stop("Requested AOI not in model domain")
      }
    }
  ))
  
  bb = st_bbox(bb)
  
  if(all(sf::st_geometry_type(AOI) != "POINT")) {
    
    ys = c(which.min(abs(Y_coords - bb$ymax)), which.min(abs(Y_coords - bb$ymin)))
    xs = c(which.min(abs(X_coords - bb$xmin)), which.min(abs(X_coords - bb$xmax)))
    
    g = list(ymax = max(ys), ymin = min(ys), xmax = max(xs), xmin = min(xs))
    g[['type']] = 'grid'
    g[['lat.call']] = paste0('[', g$ymin - 1 , ':1:', g$ymax - 1, ']')
    g[['lon.call']] = paste0('[', g$xmin - 1,  ':1:', g$xmax - 1, ']')
    
    xrange = range(X_coords[c(g$xmin:g$xmax)])
    xrange[1] = xrange[1] - abs(.5*grid$dX)
    xrange[2] = xrange[2] + abs(.5*grid$dX)
    yrange = range(Y_coords[c(g$ymin:g$ymax)])
    yrange[1] = yrange[1] - abs(.5*grid$dY)
    yrange[2] = yrange[2] + abs(.5*grid$dY)
    
    g['rows'] = abs(diff(yrange) / grid$dY)
    g['cols'] = abs(diff(xrange) / grid$dX)
    g[['e']] = raster::extent(xrange, yrange)
    g[['proj']] = grid$proj
    g[["base"]] = grid$base
    
   b =  st_transform(AOI, grid$proj) %>% st_bbox()
    b$ymax - b$ymin
  } else {
    
    lon = bb$xmin
    lat = bb$ymin
    
    g = list(y = which.min(abs(Y_coords  - lat)),
             x = which.min(abs(X_coords  - lon)))
    
    g[["lat"]]      = Y_coords[g$y]
    g[["lon"]]      = X_coords[g$x]
    g[['type']]     = 'point'
    g[['lat.call']] = paste0('[', g$y - 1 , ':1:', g$y - 1, ']')
    g[['lon.call']] = paste0('[', g$x - 1,  ':1:', g$x - 1, ']')
    g[['e']] = NULL
    g[['proj']] = grid$proj
    g[["base"]] = grid$base
    
  }
  
  return(g)
}


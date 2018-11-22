define.grid = function(AOI, service = NULL, proj = '+proj=longlat +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +no_defs'){

  load('/Users/mikejohnson/Documents/GitHub/climateR/data/grids.rda')
  grid = eval(parse(text = paste0('grids$', service)))

if(grepl(paste('raster', 'Spatial', 'sf', sep = "|"), class(AOI))) {

  AOI = sp::spTransform(AOI, proj)

  bb = AOI::bbox_st(AOI)

  if(sum(c(
    bb$xmin > min(grid$long),
    bb$xmax < max(grid$long),
    bb$ymin > min(grid$lat),
    bb$ymax < max(grid$lat)
    )) != 4) {stop('AOI not within ', toupper(service), ' domain')}

  g = list(
    ymax = max(which.min(abs(grid$lat  - bb$ymax)), which.min(abs(grid$lat  - bb$ymin))),
    ymin = min(which.min(abs(grid$lat  - bb$ymax)), which.min(abs(grid$lat  - bb$ymin))),
    xmax = max(which.min(abs(grid$long - bb$xmin)), which.min(abs(grid$long - bb$xmax))),
    xmin = min(which.min(abs(grid$long - bb$xmin)), which.min(abs(grid$long - bb$xmax)))
  )

  g[['type']] = 'grid'
  g[['lat.call']] = paste0('[', g$ymin, ':1:', g$ymax, ']')
  g[['lon.call']] = paste0('[', g$xmin, ':1:', g$xmax, ']')
  g[['e']] = raster::extent(range(grid$long[c(g$xmin:g$xmax)]), range(grid$lat[c(g$ymin:g$ymax)]))
  g[['AOI']] = AOI

} else {

  if(class(AOI) != 'data.frame'){ AOI = data.frame(lat = AOI[1], lon = AOI[2])}

  if(sum(c(
    AOI$lon > min(grid$long),
    AOI$lon < max(grid$long),
    AOI$lat > min(grid$lat),
    AOI$lat < max(grid$lat)
  )) != 4) {stop('Requested Point not within ', toupper(service), ' domain')}

  g = list(
    y = which.min(abs(grid$lat  - AOI$lat)),
    x = which.min(abs(grid$long  - AOI$lon))
  )

  g[["lat"]]      = grid$lat[g$y]
  g[["lon"]]      = grid$lon[g$x]
  g[['type']]     = 'point'
  g[['lat.call']] = paste0('[', g$y, ':1:', g$y, ']')
  g[['lon.call']] = paste0('[', g$x, ':1:', g$x, ']')
  g[["e"]]        = NULL
  g[['AOI']]      = sp::SpatialPointsDataFrame(coords = cbind(AOI$lon, AOI$lat), data = AOI)

}

  return(g)

}


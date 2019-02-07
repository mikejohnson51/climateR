input = getAOI(state = 'NC', county = 'all')

cell = cell_geometry %>% as_Spatial()

clim = getPRISM(cell, "prcp", startDate = "2018-10-11")

r = clim[[1]]

g = make_grid(r)

plot(g[[1]])
plot(g[g$grid_ids %in% c(100:200),], col = 'red', add = T)
plot(g[g$grid_ids == 2,], col = 'red', add = T)
plot(g[g$grid_ids == 3,], col = 'red', add = T)


w.dave = get_weights(cell_geometry, geom, "FIPS")

w = get_weights(g, input, "countyfp")


z = zonal_stats(r, input, w)

plot(z[ ,grepl("X2018", names(z))])


raster::plot(r[[1]])
sp::plot(input, add = T)
####
####
####

make_grid = function(r){

lon <- raster::getValues(raster::init(r[[1]], 'x'))
lat <- raster::getValues(raster::init(r[[1]], 'y'))
bb <- list(cbind(c(min(lon), max(lon) , min(lon)), c(min(lat), max(lat), min(lat))))
y.dim = dim(r)[1]
x.dim = dim(r)[2]
t.dim = dim(r)[3]
ids = matrix(c(1:(x.dim*y.dim)), nrow = y.dim, ncol = x.dim, byrow = F)
prj   = as.character(r@crs)

e <- sf::st_sf(sf::st_sfc(sf::st_polygon(bb)))

grd_lrg <- sf::st_make_grid(e, n = c(x.dim, y.dim)) %>%
           sf::st_set_crs(prj) %>%
           sf::st_sf()

#grd_lrg$grid_ids = as.vector(t(apply(ids, 2, rev)))

grd_lrg$grid_ids = as.vector(t(apply(ids, 2, rev)))

# g = grd_lrg
#
# plot(g[[1]])
# plot(g[g$grid_ids == 1,], col = 'red', add = T)
# plot(g[g$grid_ids == 2,], col = 'red', add = T)
# plot(g[g$grid_ids == 3,], col = 'red', add = T)
#
# plot(g[g$grid_ids == 33,], col = 'red', add = T)


return(grd_lrg)

}

get_weights = function(grid, poly, abb = NULL){

if(is.null(abb)){ abb = names(poly)[1]}

if(!AOI::checkClass(poly, "sf")) { poly = sf::st_as_sf(poly) }

if(st_crs(poly) != st_crs(grid)) { poly  <- st_transform(poly, st_crs(grid)) }

if(abb %in% names(poly)){ poly = poly[,abb] } else { stop(paste(abb, "is not an attribute of polygon input"))}

sf::st_agr(poly) <- "constant"
sf::st_agr(grid) <- "constant"

int <- suppressMessages ( areal::aw_intersect(poly, source = grid, areaVar = "area") )

int <- areal::aw_total(int, source = grid, id = "grid_ids", areaVar = "area",
                         totalVar = "totalArea", type = "extensive", weight = "total")

int <- areal::aw_weight(int, areaVar = "area", totalVar = "totalArea",
                          areaWeight = "areaWeight")

int <- merge(st_set_geometry(int, NULL), st_set_geometry(grid,  NULL), by = "grid_ids",  all.y = T)

int = int[,c("grid_ids", abb, "areaWeight")]

return(int)
}

zonal_stats = function(r, poly, area_weights){

abb = names(poly)[2]

if(!AOI::checkClass(poly, "sf")) { poly = sf::st_as_sf(poly) }

t.dim = dim(r)[3]
y.dim = dim(r)[1]
x.dim = dim(r)[2]
t.dim = dim(r)[3]
ids = matrix(c(1:(x.dim*y.dim)), nrow = y.dim, ncol = x.dim, byrow = T)


names(area_weights) <- c("grid_ids", "poly_id", "w")

  rows <- sum(!is.na(unique(area_weights$poly_id)))

  df = data.frame(matrix(vector(), rows, t.dim + 1,
                         dimnames=list(c(), c('poly_id', names(r)))),
                          stringsAsFactors=F)

  for (i in 1:t.dim) {
      try_backoff({

        i_data <- data.frame(d = raster::getValues(r[[i]]), grid_ids = matrix(ids, ncol = 1))

        i_data_mine = merge(i_data, area_weights, by = "grid_ids", all.y = T, all.x = T)

        i_data_mine <- by(i_data_mine, list(i_data_mine$poly_id), function(x)
          d = (sum( (x$d * x$w), na.rm = T) /
                 sum(x$w, na.rm = T)))

        df[, i + 1 ] <- do.call(c, list(i_data_mine))

    })
  }

  df$poly_id =  names(i_data_mine)

  return(merge(x = poly, y = df, by.x = abb, by.y = 'poly_id'))
}


###
###

try_backoff <- function(expr, silent=FALSE, max_attempts=10, verbose=FALSE) {
  for (attempt_i in seq_len(max_attempts)) {
    results <- try(expr = expr, silent = silent)
    if (class(results) == "try-error") {
      backoff <- runif (n = 1, min = 0, max = 2 ^ attempt_i - 1)
      if (verbose) {
        message("Backing off for ", backoff, " seconds.")
      }
      Sys.sleep(backoff)
    } else {
      if (verbose) {
        message("Succeeded after ", attempt_i, " attempts.")
      }
      break
    }
  }
  results
}

#####
#####
#####
#####


nc_file <- "https://cida.usgs.gov/thredds/dodsC/prism_v2"
nc_var <- ncmeta::nc_vars(nc_file)
variable_name <- "ppt"
nc_coord_vars <- ncmeta::nc_coord_var(nc_file, variable_name)


geom <- sf::st_transform(sf::read_sf(system.file("shape/nc.shp",
                                                  package = "sf")),
                          "+init=epsg:5070")

nc_prj <- ncmeta::nc_gm_to_prj(ncmeta::nc_grid_mapping_atts(nc_file))

nc <- RNetCDF::open.nc(nc_file)
col_coords <- RNetCDF::var.get.nc(nc, nc_coord_vars$X, unpack = TRUE)

col_coords <- seq(from = col_coords[1],
                  to = col_coords[length(col_coords)],
                  along.with = col_coords)

row_coords <- RNetCDF::var.get.nc(nc, nc_coord_vars$Y, unpack = TRUE)

row_coords <- seq(from = row_coords[1],
                  to = row_coords[length(row_coords)],
                  along.with = row_coords)

cell_geometry <-
    intersectr::create_cell_geometry(col_coords = col_coords,
                         row_coords = row_coords,
                         prj = nc_prj,
                         geom = geom,
                         buffer_dist = 10000)


plot(cell_geometry$geometry)
plot(cell_geometry$geometry[cell_geometry$grid_ids == 70], add = T, col = 'blue')

plot(g[[1]])
plot(g[g$grid_ids %in% c(1),], col = 'red', add = T)
plot(g[g$grid_ids == 2,], col = 'red', add = T)
plot(g[g$grid_ids == 3,], col = 'red', add = T)



data_source_cells <- sf::st_sf(dplyr::select(cell_geometry, grid_ids))
target_polygons <- sf::st_sf(dplyr::select(geom, FIPS))
sf::st_agr(data_source_cells) <- "constant"
sf::st_agr(target_polygons) <- "constant"
area_weights <- intersectr::calculate_area_intersection_weights(
  data_source_cells,
  target_polygons)



a = area_weights[!is.na(area_weights$FIPS),]
wd = w.dave[!is.na(w.dave$FIPS),]

head(a[order(a$grid_ids),], 20)
head(wd[order(wd$grid_ids),], 20)

?st_agr
sf::st_agr(g)

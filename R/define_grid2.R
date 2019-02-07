#' @title Define AOI native grid
#' @description This functions defines the grid of the AOI in terms of the climate resource
#' @keywords internal
#' @param AOI an AOI object
#' @param url a URL to the home OPENDaP resource
#' @return a list of values including type, lat.call, lon.call, extent and AOI
#' @export
#' @author Mike Johnson

define.grid2 = function(AOI, url){

nc = RNetCDF::open.nc(url)
nc_prj <- suppressWarnings( ncmeta::nc_gm_to_prj(ncmeta::nc_grid_mapping_atts(nc)) )

if(!AOI::checkClass(AOI, "sf")) { AOI = sf::st_as_sf(AOI) }

bb = AOI %>% sf::st_transform(nc_prj) %>% AOI::bbox_st()

nc_var <- ncmeta::nc_vars(nc)

variable_name <- nc_var$name[length(nc_var$name)]
nc_coord_vars <- ncmeta::nc_coord_var(nc, variable_name)
X_coords <- RNetCDF::var.get.nc(nc, nc_coord_vars$X, unpack = TRUE)
if(max(X_coords) > 180) {X_coords = X_coords - 360}

Y_coords <- RNetCDF::var.get.nc(nc, nc_coord_vars$Y, unpack = TRUE)

g = list(
  ymax = max(which.min(abs(Y_coords  - bb$ymax)), which.min(abs(Y_coords - bb$ymin))),
  ymin = min(which.min(abs(Y_coords  - bb$ymax)), which.min(abs(Y_coords  - bb$ymin))),
  xmax = max(which.min(abs(X_coords - bb$xmin)),  which.min(abs(X_coords - bb$xmax))),
  xmin = min(which.min(abs(X_coords - bb$xmin)),  which.min(abs(X_coords - bb$xmax)))
)

g[['type']] = 'grid'
g[['lat.call']] = paste0('[', g$ymin - 1 , ':1:', g$ymax - 1, ']')
g[['lon.call']] = paste0('[', g$xmin - 1,  ':1:', g$xmax - 1, ']')
g[['e']] = raster::extent(range(X_coords[c(g$xmin:g$xmax)]), range(Y_coords[c(g$ymin:g$ymax)]))
g[['AOI']] = AOI

return(g)

}


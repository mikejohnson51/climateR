#' ClimateR Catalog
#' @family catalog

"catalog"

NULL

#' @importFrom arrow read_parquet
#' @importFrom dplyr filter mutate select distinct `%>%` everything mutate_all bind_rows left_join rename group_by ungroup slice slice_sample
#' @importFrom glue glue
#' @importFrom terra intersect project vect crs ext relate rast crop flip `ext<-` `crs<-` `units<-` `time<-` union sprc merge units nlyr as.polygons extract time align ymax ymin xmax xmin plot minmax setGDALconfig is.related is.points geomtype unique
#' @importFrom RNetCDF open.nc close.nc var.get.nc dim.inq.nc var.inq.nc utcal.nc att.get.nc 
#' @importFrom future.apply future_lapply
#' @importFrom ncmeta nc_coord_var nc_grid_mapping_atts nc_gm_to_prj nc_vars nc_var nc_dims
#' @importFrom utils head tail
#' @importFrom gifski save_gif
#' @importFrom methods formalArgs
#' @importFrom stats complete.cases
#' @importFrom grDevices blues9


#' @export
terra::plot

#' @export
dplyr::`%>%`




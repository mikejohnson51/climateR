#' @title Faster Raster Re-projection
#' @description Uses the `sf` GDAL bindings to provide fast raster reprojection. 
#' Currently we are only supporting the ability to transform the grid reference 
#' system, not the grid resolution. 
#' @param r a single or list of rasterLayer or rasterStack objects
#' @param target_prj the target projection of the output raster. 
#' Can be either a proj4string, or an EPSG code
#' @param method the method for resampling/reprojecting. Default is 'bilinear'. 
#' Options can be found [here](https://gdal.org/programs/gdalwarp.html#cmdoption-gdalwarp-r)
#' @export
#' @importFrom sf st_crs gdal_utils
#' @importFrom raster writeRaster
 
fast_reproject = function(r, target_prj, method = "bilinear"){

  .reproject  <- function(r, target_prj, method = "bilinear"){
    destfile  <- tempfile(fileext = ".tif")
    destfile2 <- tempfile(fileext = ".tif")
    names     <-  names(r)
    
    raster::writeRaster(r, destfile, format="GTiff")
  
    sf::gdal_utils(util = "warp", 
                   source = destfile, 
                   destination = destfile2,
                   options = c("-t_srs", as.character(sf::st_crs(target_prj)$proj4string),
                               "-r", method)
    )
    
    out <-  stack(destfile2)
    names(out) <-  names(r)
    file.remove(destfile)
    out
  } 

  out <-  lapply(1:length(r), function(x){.reproject(r[[x]], target_prj, method)}) 
  
  names(out) <- names(r)
  
  out
}




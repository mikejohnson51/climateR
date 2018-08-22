#' Get NWM gridded data output
#'
#' Download land output from the HydroShare Thredds server for a list of input files.
#' Parallized version of \code{get_land_data} using \code{doParallel} and \code{foreach}.
#'
#' @param AOI a AOI to subset data (generated with AOI::getAOI())
#' @param filelist a list of filepaths (generated with getFiles)
#' @param param a channel paramater
#' @param layer for soil and snow land parameters a layer must be declared (defaults to 1)
#'
#' @return a raster stack
#' @export
#'

getGridded = function(AOI, filelist, param, layer = NULL) {

  i = NULL

  types  = c("channel", "land", "forcing", "terrain", "reservoir")

  type = types[sapply(types, grepl, filelist[1])]

  if(type == 'terrain') { param = tolower(param)} else { param = toupper(param) }

  `%dopar%` <- foreach::`%dopar%`

  combine_lists <- function(LL1, LL2) {
    r <- raster::stack(LL1$r, LL2$r)
    time <- c(LL1$time, LL2$time)
    return(list(r = r, time = time))
  }

  layer = defineLayers(param, layer)

  grid = define_AOI_grid(AOI)

  no_cores <- parallel::detectCores() - 1
  doParallel::registerDoParallel(no_cores)

  res <-
    foreach::foreach(i = 1:length(filelist) , .combine = combine_lists) %dopar% {

      file = paste0(filelist[i],
                    "?time[0:1:0],",
                    param,
                    "[0:1:0][",
                    grid$y.min,
                    ":1:",
                    grid$y.max ,
                    "]",
                    layer$layer.p,
                    "[",
                    grid$x.min,
                    ":1:",
                    grid$x.max,
                    "]"
      )

      nc = ncdf4::nc_open(file)
      vals = ncdf4::ncvar_get(nc, param)
      time = ncdf4::ncvar_get(nc, "time")
      ncdf4::nc_close(nc)

      mat <- apply(t(vals),2,rev)
      r <-raster::raster(mat)
      raster::projection(r) = sp::CRS("+proj=lcc +lat_1=30 +lat_2=60 +lat_0=40 +lon_0=-97 +x_0=0 +y_0=0 +a=6370000 +b=6370000 +units=m +no_defs")
      raster::extent(r) = c(grid$long.min, grid$long.max, grid$lat.min, grid$lat.max)
      raster::res(r) = 1000

      return(list(r = r, time = time))

    }

  names(res$r) <- paste0("X.", res$time)

  return(res$r)

}

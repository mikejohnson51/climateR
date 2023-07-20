#' @title Animate Object as GIF
#' @description
#' Animate a SpatRaster object as a gif.
#' @inheritParams animation_vector
#' @inheritParams animation_raster
#' @return file.path
#' @family viz
#' @export
#' 

animation = function(data, AOI = NULL, feild_pattern = NULL, outfile, colors = blues9){
  
  if(inherits(data, "SpatRaster")){
   x =  animation_raster(data, AOI, outfile, colors)
  } else {
   x = animation_vector(data, feild_pattern, outfile, colors)
  }
  
  return(x)
  
}


#' @title Animate vector as GIF
#' @description
#' Animate a sf or SpatVect object as a gif.
#' @param data a SpatVect or sf object
#' @param feild_pattern optional string vector to filter the desired attributes by
#' @param outfile path to write gif file, must have .gif extenstion
#' @param colors colors to plot with
#' @return file.path
#' @family viz

animation_vector = function(data, feild_pattern = NULL, outfile, colors = blues9){

  data = spatAOI(data)
  d = data[, grepl(feild_pattern, names(data))]
  x = as.data.frame(d)[,grepl(feild_pattern, names(d))]
  y = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
  
  brk <- seq(min(y), max(y), (max(y) - min(y)) / length(colors))
  n = names(d)[grepl(feild_pattern, names(d))]
  
  gifski::save_gif({
    for (t in 1:length(n)) {
      try({
       plot(
          d,
          n[t],
          breaks = brk,
          col = colors ,
          legend = T,
          axes = F,
          box = F,
          main = n[t]
          )
      }, silent = FALSE)
    }
  }, gif_file = outfile, width = 800, height = 600, delay = .5, loop = TRUE)
  
  return(outfile)
}


#' @title Animate SpatRast as GIF
#' @description
#' Animate a SpatRaster object as a gif.
#' @param data a single SpatRast object
#' @param AOI optional AOI sf or SpatVect object to overlay on gif
#' @param outfile path to write gif file, must have .gif extenstion
#' @param colors colors to plot with
#' @return file.path
#' @family viz


animation_raster = function(data, AOI = NULL, outfile, colors = blues9){
  
  y = terra::minmax(data, compute = TRUE)
  
  y = y[!is.na(y)]
  
  brk <- seq(min(y), max(y), (max(y) - min(y)) / length(colors))
  n = names(data)
  
  gifski::save_gif({
    for (t in 1:nlyr(data)) {
      try({
        terra::plot(
          data[[t]],
          breaks = brk,
          col = colors ,
          legend = T,
          axes = F,
          box = F,
          main = n[t]
        )
        if(!is.null(AOI)){
          plot(spatAOI(AOI), col = NULL, add = TRUE)
        }
        
      }, silent = F)
    }
  }, gif_file = outfile, width = 800, height = 600, delay = .5, loop = TRUE)
  
  return(outfile)
}

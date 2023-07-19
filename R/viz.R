#' @title Animate SpatRast as GIF
#' @description
#' Animate a SpatRaster object as a gif.
#' @param data a single SpatRast object
#' @param AOI optional AOI sf or SpatVect object to overlay on gif
#' @param outfile path to write gif file, must have .gif extenstion
#' @param colors colors to plot w
#' @return file.path
#' @family viz
#' @export

animation = function(data, AOI = NULL, outfile, colors = blues9){
  
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

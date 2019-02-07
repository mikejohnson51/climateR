#' @title Get PRISM Climate Data for an Area of Interest
#' @description The Parameter-elevation Regressions on Independent Slopes Model (PRISM) group gathers climate observations
#' from a wide range of monitoring networks, applies sophisticated quality control measures, and develops spatial climate datasets to reveal
#' short- and long-term climate patterns. The resulting datasets incorporate a variety of modeling techniques and are available at multiple spatial/temporal
#' resolutions, covering the period from 1895 to the present reflects observed patterns of daily near-surface meteorology and simulated changes in GCMs experiments.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$prism`)
#' @param model GMC model name (see `model_meta$prism$name`)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return a list of rasterStacks
#' @export


getPRISM = function(AOI, param, startDate, endDate = NULL){

  d = define.dates  (startDate, endDate)
  p = define.param  (param, service = 'prism')

  tmp = expand.grid(string = d$string, call = p$call, stringsAsFactors = F)
  fin = merge(tmp, p, "call") %>% merge(d, "string")

  base = 'http://convection.meas.ncsu.edu:8080/thredds/dodsC/prism/daily/combo/'
  call = paste0(fin$year[1], '/PRISM_combo_', fin$string[1], '.nc')

  g = suppressWarnings( define.grid2(AOI, url = paste0(base,call)) )
  s = define.initial(g, d)

  for(i in 1:NROW(fin)){

      call = paste0(fin$year[i], '/PRISM_combo_', fin$string[i], '.nc?', fin$call[i], '[0:1:0]')

      nc = RNetCDF::open.nc(paste0(base, call, g$lat.call,  g$lon.call))

      var = RNetCDF::var.get.nc(nc, fin$call[i])

      RNetCDF::close.nc(nc)

      s = process.var(group = s, g = g, var, fun = 't', dates = fin$date[i],
                      param = fin$common.name[i], name = fin$date[i], proj = "+init=epsg:4269")
  }

  ss = define.initial(grid = g, date = d)

  for(i in 1:NROW(p)){ ss[[p$common.name[i]]] = raster::stack(s[grepl(p$common.name[i], names(s))]) }

  return(ss)
}

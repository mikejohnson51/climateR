#' @title Get PRISM Climate Data for an Area of Interest
#' @description The Parameter-elevation Regressions on Independent Slopes Model (PRISM) group gathers climate observations
#' from a wide range of monitoring networks, applies sophisticated quality control measures, and develops spatial climate datasets to reveal
#' short- and long-term climate patterns. The resulting datasets incorporate a variety of modeling techniques and are available at multiple spatial/temporal
#' resolutions, covering the period from 1895 to the present reflects observed patterns of daily near-surface meteorology and simulated changes in GCMs experiments.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$prism`)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export

getPRISM = function(AOI, param, startDate, endDate = NULL){

  id = 'prism'

  d = define.dates  (startDate, endDate)
  p = define.param  (param, service = id)
  g = define.grid3(AOI, id)

  tmp = expand.grid(string = d$string, call = p$call, stringsAsFactors = F)
  fin = merge(tmp, p, "call") %>% merge(d, "string")
  fin = fin[order(fin$call),]

  urls = paste0(g$base, fin$year, "/PRISM_combo_", fin$string, ".nc?", fin$call, "[0:1:0]", g$lat.call, g$lon.call)

  s = fast.download(urls, params = fin$call, names = fin$common.name, g, date.names = unique(fin$date), dataset = id, fun = "t", no_data = -1000)
  s
}


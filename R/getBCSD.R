#' @title Get BCSD VIC outputs Climate Data for an Area of Interest
#' @description Multivariate Adaptive Constructed Analogs (MACA) is a statistical method for downscaling Global Climate Models
#' (GCMs) from their native coarse resolution to a higher spatial resolution that captures
#' reflects observed patterns of daily near-surface meteorology and simulated changes in GCMs experiments.
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$maca`)
#' @param model GMC model name (see `model_meta$maca$name`)
#' @param scenario a climate scenario pathway (rcp45 or rcp85)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return a list of rasterStacks
#' @export

getBCSDvic = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

d = define.dates(startDate, endDate, baseDate = '1950-01-15')
p = define.param(param, service = 'bcsd')
k = define.config(dataset = 'bcca', model = model)

fin = expand.grid(model = k, scenario = scenario, call = p$call)
fin = merge(fin, p, "call")

base = "https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC?"

g = define.grid2(AOI, url = base)
s = define.initial(g, d)


for(i in 1:NROW(fin)){

nc = ncdf4::nc_open(paste0("https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC?",
                           tolower(fin$model[i]),
                           "_",
                           fin$scenario[i],
                           "_r1i1p1_",
                           fin$call[i],
                           "[", min(d$month.index), ":1:" , max(d$month.index), "]",
                           g$lat.call,
                           g$lon.call))

var = ncdf4::ncvar_get(nc)
ncdf4::nc_close(nc)

s = process.var(group = s, g = g, var,  dates = unique(format(d$date, "%Y-%m")),
                param = fin$common.name[i], name = paste0(fin$model[i], "_", fin$scenario[i]))
}

return(s)
}


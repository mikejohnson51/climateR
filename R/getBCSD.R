#' @title Get BCSD Hydrologic (VIC) Outputs for an Area of Interest
#' @description CMIP5 Monthly Hydrology Projections driven with BCSD climate forcings
#' @param AOI a spatial polygon object (sf or sp)
#' @param param a meterological parameter (see `param_meta$bcsd`)
#' @param model GMC model name (see `model_meta$bcsd`)
#' @param scenario a climate scenario pathway (rcp45 or rcp85)
#' @param startDate a start date given as "YYYY-MM-DD"
#' @param endDate   an end date given as "YYYY-MM-DD"
#' @author Mike Johnson
#' @return if AOI is an areal extent a list of rasterStacks, if AOI is a point then a data.frame of modeled records.
#' @export

getBCSDvic = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

id = "bcsd"
base = "https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC?"

d = define.dates(startDate, endDate, baseDate = '1950-01-15')
p = define.param(param, service = 'bcsd')
k = define.config(dataset = 'bcca', model = model)
g = define.grid3(AOI, id)

fin = expand.grid(model = k, scenario = scenario, call = p$call)
fin = merge(fin, p, "call")

var.names = paste0(tolower(fin$model), "_",  fin$scenario, "_r1i1p1_",fin$call)

urls = paste0(base, var.names,
              "[", min(d$month.index), ":1:" , max(d$month.index), "]",
              g$lat.call,
              g$lon.call )


s = fast.download(urls, params = var.names, names = paste0(fin$model, "_", fin$common.name), g, date.names = unique(format(d$date, "%Y-%m")), dataset = id)

s
}



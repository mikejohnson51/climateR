getBCSD = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

d = define.dates(startDate, endDate, baseDate = '1950-01-15')
g = define.grid(AOI,    service = 'bcsd')
p = define.param(param, service = 'bcsd')
s = define.initial(g, d)

vals = expand.grid(model = model, scenario = scenario, param = p$call)

for(i in 1:NROW(vals)){

nc = ncdf4::nc_open(paste0("https://cida.usgs.gov/thredds/dodsC/BCSD_mon_VIC?",
                           tolower(vals$model[i]),
                           "_",
                           vals$scenario[i],
                           "_r1i1p1_",
                           p$call,
                           "[", min(d$month.index), ":1:" , max(d$month.index), "]",
                           g$lat.call,
                           g$lon.call))

var = ncdf4::ncvar_get(nc)
ncdf4::nc_close(nc)

s = process.var(group = s, g = g, var,  dates = unique(format(d$date, "%Y-%m")), param = paste(vals$param[i], vals$model[i], vals$scenario[i], sep = "_"))
}

return(s)
}

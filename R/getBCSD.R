getBCSDvic = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

d = define.dates(startDate, endDate, baseDate = '1950-01-15')
g = define.grid(AOI,    service = 'bcsd')
p = define.param(param, service = 'bcsd')
s = define.initial(g, d)
k = define.config(dataset = 'bcca', model = model)

fin = expand.grid(model = k, scenario = scenario, call = p$call)
fin = merge(fin, p, "call")

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

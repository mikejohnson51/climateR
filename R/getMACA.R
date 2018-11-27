getMACA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

  d = define.dates(startDate, endDate)

  versions = data.frame(matrix(ncol = 5, nrow = 0))

  future = d[(as.Date(d$date) > as.Date("2005-12-31")),]
  f.date.index = future$date -  as.Date('2006-01-01')

  if(length(future$date) != 0){
  for(i in scenario){
    tmp = cbind(dates = future$date,
                time.index = paste0(paste0("[", min(f.date.index) , ":1:", max(f.date.index), "]")),
                calls = paste0(i, '_2006_2099_CONUS_daily.nc?'),
                diff = 0,
                ver = i)
    versions[NROW(versions) + 1, ] = tmp
  }
  }

  historic = d[as.Date(d$date) < as.Date("2006-01-01"),]
  h.date.index = historic$date - as.Date('1950-01-01')

  if(length(historic$date) != 0){

      min.d = min(h.date.index)
      diff  = 0
    if(min.d > 20418){
      diff  = as.numeric(min.d - 20418)
      min.d = 20418
    }

    tmp = cbind(dates = historic$date,
                time.index = paste0(paste0("[", min.d , ":1:", max(h.date.index), "]")),
                calls = 'historical_1950_2005_CONUS_daily.nc?',
                diff = diff,
                ver = 'historic')

    versions[NROW(versions) + 1, ] = tmp
  }

  colnames(versions) = c("dates", "time.index", "calls", "diff", "ver")

  g = define.grid(AOI, service = 'gridmet')
  p = define.param(param, service = 'maca')
  s = define.initial(g, d)
  vals = define.config(dataset = "maca", model = model, ensemble = NA)

  tmp = expand.grid(dates = versions$dates, model = vals, call = p$call)
  fin = merge(versions, tmp, "dates")
  fin = merge(fin, p, "call")

for(i in 1:NROW(fin)){

    base =  "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_"

    def  =  paste0(fin$call[i], "_", fin$model[i], ifelse(fin$model[i] == 'CCSM4', '_r6i1p1_', '_r1i1p1_'))

    nc = ncdf4::nc_open(paste0(base, def, fin$calls[i], fin$call2[i], fin$time.index[i], g$lat.call, g$lon.call))

    var = ncdf4::ncvar_get(nc, fin$call2[i])

    if(fin$diff[i] != 0){ var = var[,,-c(1:fin$diff[i])] }

    ncdf4::nc_close(nc)

    s = process.var(group = s, g = g, var, dates = fin$dates[i], param = paste0(fin$common.name[i], "_", fin$ver[i], "_", fin$model[i]))
  }

  return(s)

}



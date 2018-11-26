AOI = getAOI(state = "CA")
            model = "CCSM4"
            param = 'prcp'
            scenario = c('rcp45', 'rcp85')
            startDate = "2080-06-29"
            endDate = "2080-06-30"

getMACA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

  d = define.dates(startDate, endDate)

  versions = list()

  future = d[(as.Date(d$date) > as.Date("2005-12-31")),]
  f.date.index = future$date -  as.Date('2006-01-01')


  if(length(future$date) != 0){
  for(i in scenario){
    versions[[i]] = list(dates = future$date,
                         time.index = paste0(paste0("[", min(f.date.index) , ":1:", max(f.date.index), "]")),
                         calls = paste0(i, '_2006_2099_CONUS_daily.nc?'),
                         diff = 0,
                         ver = i)
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

  versions[['historic']] = list(dates = historic$date,
                                time.index = paste0(paste0("[", min.d , ":1:", max(h.date.index), "]")),
                                calls = 'historical_1950_2005_CONUS_daily.nc?',
                                diff = diff,
                                ver = 'historic')
  }

  g = define.grid(AOI, service = 'gridmet')
  p = define.param(param, service = 'maca')
  s = define.initial(g, d)

for(i in 1:NROW(p)){

  tt = define.initial(g, d)

  for( v in 1:length(versions)){

    curr = versions[[v]]

    base =  "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_macav2metdata_"

    def  =  paste0(p$call[i], "_", model,ifelse(model == 'CCSM4', '_r6i1p1_', '_r1i1p1_'))

    nc = nc_open(paste0(base, def, curr$calls, p$call2[i], curr$time.index, g$lat.call, g$lon.call))

    var = ncvar_get(nc, p$call2[i])

    if(curr$diff != 0){ var = var[,,-c(1:curr$diff)] }

    nc_close(nc)

    s = process.var(group = s, g = g, var, dates = curr$dates, param = paste0(p$common.name[i], "_", curr$ver))
  }

  # names = names(tt)
  # curr.names = names[grepl(p$common.name[i], names)]
  # hist = which(grepl('historic', curr.names))
  # rcp45 = which(grepl('45', curr.names))
  # rcp85 = which(grepl('85', curr.names))

  # ## build RCP45
  #
  # if(all(length(hist) != 0, length(rcp45) != 0)) {
  #   s[[paste0(p$common.name[i], "_rcp45")]] = stack(tt[[hist]], tt[[rcp45]])
  # } else if(length(rcp45) !=0) {
  #   s[[paste0(p$common.name[i], "_rcp45")]] = tt[[rcp45]]
  # }
  #
  # ## build RCP85
  #
  # if(all(length(hist) != 0, length(rcp85) != 0)) {
  #   s[[paste0(p$common.name[i], "_rcp85")]] = stack(tt[[hist]], tt[[rcp85]])
  # } else if(length(rcp45) !=0) {
  #   s[[paste0(p$common.name[i], "_rcp45")]] = tt[[rcp45]]
  # }
  #
  # ## build historical
  #
  # if(all(length(rcp45) == 0, length(rcp85) == 0, length(hist) != 0 )) { s[[paste0(p$common.name[i], "_historical")]] = tt[[hist]] }

  }

  if(g$type == 'grid') { s[['AOI']] = g$AOI }

  return(s)

}


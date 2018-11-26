getBCCA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

  d = define.dates(startDate, endDate)

  versions = list()

  future = d[(as.Date(d$date) > as.Date("2005-12-31")),]
  f.date.index = future$date - as.Date('2006-01-01')

  if(length(future$date) != 0){
    for(i in scenario){
      versions[[i]] = list(dates = future$date,
                           time.index = paste0(paste0("[", min(f.date.index) , ":1:", max(f.date.index), "]")),
                           holding = 'future?',
                           ver = paste0("_", i, "_"))
    }
  }

  historic = d[as.Date(d$date) < as.Date("2006-01-01"),]
  h.date.index = historic$date - as.Date('1950-01-01')

  if(length(historic$date) != 0){
    h.date.index = historic$date - as.Date('1950-01-01')

    versions[['historic']] = list(dates = historic$date,
                                  time.index = paste0(paste0("[", min(h.date.index) , ":1:", max(h.date.index), "]")),
                                  holding = 'historical?',
                                  ver = 'historical')
  }

  g = define.grid(AOI, service = 'bcca')
  p = define.param(param, service = 'bcca')
  s = define.initial(g, d)

  for(i in 1:NROW(p)){

    tt = define.initial(g, d)

    for( v in 1:length(versions)){

      curr = versions[[v]]

      base =  "https://cida.usgs.gov/thredds/dodsC/cmip5_bcca/"
      base1 = "BCCA_0-125deg_"
      base2 = "_day_"

      ensemble  =  'r1i1p1'

      nc = ncdf4::nc_open(paste0(base,
                          curr$holding,
                          base1,
                          p$call[i],
                          base2,
                          model, "_",
                          curr$ver, "_",
                          ensemble,
                          curr$time.index,
                          g$lat.call,
                          g$lon.call))

      var = ncdf4::ncvar_get(nc)

      ncdf4::nc_close(nc)

      tt = process.var(group = tt, g = g, var, dates = curr$dates, param = paste0(p$common.name[i], "_", curr$ver))
    }

    names = names(tt)
    curr.names = names[grepl(p$common.name[i], names)]
    hist = which(grepl('historic', curr.names))
    rcp45 = which(grepl('45', curr.names))
    rcp85 = which(grepl('85', curr.names))

    ## build RCP45

    if(all(length(hist) != 0, length(rcp45) != 0)) {
      s[[paste0(p$common.name[i], "_rcp45")]] = stack(tt[[hist]], tt[[rcp45]])
    } else if(length(rcp45) !=0) {
      s[[paste0(p$common.name[i], "_rcp45")]] = tt[[rcp45]]
    }

    ## build RCP85

    if(all(length(hist) != 0, length(rcp85) != 0)) {
      s[[paste0(p$common.name[i], "_rcp85")]] = stack(tt[[hist]], tt[[rcp85]])
    } else if(length(rcp45) !=0) {
      s[[paste0(p$common.name[i], "_rcp45")]] = tt[[rcp45]]
    }

    ## build historical

    if(all(length(rcp45) == 0, length(rcp85) == 0, length(hist) != 0 )) {
      s[[paste0(p$common.name[i], "_historical")]] = tt[[hist]]
    }

  }

  if(g$type == 'grid') { s[['AOI']] = g$AOI }

  return(s)

}










getLOCA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

    d = define.dates(startDate, endDate)

    versions = data.frame(matrix(ncol = 5, nrow = 0))

    future = d[(as.Date(d$date) > as.Date("2005-12-31")),]
    f.date.index = future$date - as.Date('2006-01-01')

    if(length(future$date) != 0){
      for(i in scenario){
       tmp = cbind(min.date = min(future$date),
                   max.date = max(future$date),
                   time.index = paste0(paste0("[", min(f.date.index) , ":1:", max(f.date.index), "]")),
                   calls = 'loca_future?',
                   ver = i)
       versions[NROW(versions) + 1, ] = tmp
      }
    }

    historic = d[as.Date(d$date) < as.Date("2006-01-01"),]
    h.date.index = historic$date - as.Date('1950-01-01')

    if(length(historic$date) != 0){
      h.date.index = historic$date - as.Date('1950-01-01')

      tmp = cbind(min.date = min(historic$date),
                  max.date = max(historic$date),
                  time.index = paste0(paste0("[", min(h.date.index) , ":1:", max(h.date.index), "]")),
                  calls      = 'loca_historical?',
                  ver        = 'historical')

      versions[NROW(versions) + 1, ] = tmp
    }

    colnames(versions) = c("min.date","max.date", "time.index", "calls", "ver")
    versions$min.date = as.Date(as.numeric(versions$min.date), "1970-01-01")
    versions$max.date = as.Date(as.numeric(versions$max.date), "1970-01-01")

    g = define.grid(AOI,    service = 'loca')
    p = define.param(param, service = 'loca')
    s = define.initial(g, d, p, dataset = "loca")
    vals = define.config(dataset = "loca", model = model, ensemble = NA)

    tmp = expand.grid(min.date = versions$min.date, model = vals, call = p$call)
    fin = merge(versions, tmp, "min.date")  %>% merge(p, "call") %>%  merge(model_meta$loca, "model")
    fin = fin[fin$scenario %in% scenario,]

    for(i in 1:NROW(fin)){

        base =  "https://cida.usgs.gov/thredds/dodsC/"

        def  =  paste(fin$call[i],  fin$model[i], fin$ensemble[i], sep = "_")

        nc = ncdf4::nc_open(paste0(base, fin$calls[i], def,  "_",fin$ver[i], fin$time.index[i], g$lat.call, g$lon.call))

        var = ncdf4::ncvar_get(nc)

        ncdf4::nc_close(nc)

        s = process.var(group = s, g = g, var, dates = seq.Date(fin$min.date[i], fin$max.date[i],1),
                        param = fin$common.name[i], name = paste0(fin$ver[i], "_", fin$model[i]))
      }

    return(s)

}

# grid = getAOI(list('Colorado Springs', 100, 100)) %>% getLOCA(param = 'tmax', model = 2, startDate = "2030-01-03")
# ts = geocode('Colorado Springs') %>% getLOCA(param = 'tmax', model = 2, startDate = "2030-01-01", endDate = "2030-12-31")
# plot(ts )
#
# plot(stack(grid))

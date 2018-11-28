getLOCA = function(AOI, param, model = 'CCSM4', scenario = 'rcp45', startDate, endDate = NULL){

    d = define.dates(startDate, endDate)
    v = define.versions(dates = d$date, scenario = scenario, future.call = "loca_future?", historic.call = "loca_historical?")
    g = define.grid(AOI, service = 'loca')
    p = define.param(param, service = 'loca')
    s = define.initial(g, d, p, dataset = "loca")
    k = define.config(dataset = "loca", model = model, ensemble = NA)

    tmp = expand.grid(min.date = v$min.date, model = k, call = p$call)
    fin = merge(v, tmp, "min.date")  %>% merge(p, "call") %>%  merge(model_meta$loca, "model")
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


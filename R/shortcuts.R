match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))
  
  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )
  
  match.call(sys.function(sys.parent()), call)
}

#' ClimateR dry
#' @param id The resource name, agency, or catalog identifier
#' @param args The parent function arguments
#' @param verbose Should messages be emited?
#' @param dryrun Return summary of data prior to retrieving it
#' @return data.frame

climater_dap = function(id, args, verbose, dryrun, print.arg = FALSE){
  args$id = id
  args$catalog = do.call(climater_filter, args[names(args) %in% formalArgs(climater_filter)])
  args$verbose = verbose
  
  if(dryrun){
    args$verbose = TRUE
    args$varname = NULL
    if(print.arg){print(args)}
    do.call(dap_crop, args[names(args) %in% formalArgs(dap_crop)])
  } else {
    args$varname = NULL
    if(print.arg){print(args)}
    do.call(dap, args[names(args) %in% formalArgs(dap)])
  }
}

#' @title Get Terra Climate Data for an Area of Interest
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @return if AOI is polygon a list of SpatRasters, if AOI is a point then a data.frame of modeled records.
#' @export

getTerraClim = function(AOI, varname = NULL, 
                        startDate = NULL, endDate = NULL, 
                        verbose = FALSE, dryrun = FALSE){
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap("terraclim",test(as.list(match.call.defaults()[-1])), verbose, dryrun)
}

#' @title Get Terra Climate Normals for an Area of Interest
#' @description These layers from TerraClimate were creating using climatically aided interpolation of monthly anomalies from the CRU Ts4.0 and Japanese 55-year Reanalysis (JRA-55) datasets with WorldClim v2.0 climatologies.
#' @param month numeric. and month or vector of months to access. Default is 1:12
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return
#' @export

getTerraClimNormals = function(AOI, varname, 
                               scenario = '19812010' , month = 1:12, 
                               verbose = FALSE, dryrun = FALSE){
  
  raw = climater_filter(id = "terraclim_normals", AOI = AOI, varname = varname, scenario = scenario)
  
  tmp = as.Date(strsplit(raw$duration[1], "/")[[1]])

  dates = seq.Date(tmp[1],tmp[2], by = "month")[month]
  
  if(dryrun) { 
    dap_crop(catalog = raw, AOI = AOI, startDate = head(dates, 1), endDate   = tail(dates, 1), verbose = TRUE)
  } else { 
    dap(catalog  = raw, AOI = AOI, startDate = head(dates, 1), endDate   = tail(dates, 1), verbose = verbose)
  }
}


#' @title Get Daymet Climate Data for an Area of Interest
#' @description This dataset provides Daymet Version 4 model output data as gridded estimates of daily weather parameters for North America.
#' Daymet output variables include the following parameters: minimum temperature, maximum temperature, precipitation, shortwave radiation,
#' vapor pressure, snow water equivalent, and day length. The dataset covers the period from January 1, 1980 to December 31 of the most recent full calendar year. Each subsequent year is processed individually at the close of a calendar year after
#' allowing adequate time for input weather station data to be of archive quality.  Daymet variables are continuous surfaces provided as individual files, by year, at a 1-km x 1-km spatial resolution and a daily temporal resolution. Data are in a Lambert Conformal
#' Conic projection for North America and are in a netCDF file format compliant with Climate and Forecast (CF) metadata conventions.
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return

getDaymet = function(AOI, varname = NULL, 
                     startDate = NULL, endDate = NULL, 
                     verbose = FALSE, dryrun = FALSE){
  
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap("daymet4", test(as.list(match.call.defaults()[-1])), verbose, dryrun)
}

#' @title Get MACA Climate Data for an Area of Interest
#' @description Multivariate Adaptive Constructed Analogs (MACA) is a statistical method for downscaling Global Climate Models
#' (GCMs) from their native coarse resolution to a higher spatial resolution that captures reflects observed patterns of daily near-surface meteorology and simulated changes in GCMs experiments.
#' @param timeRes daily or monthly
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @param timeRes daily or monthly
#' @inherit getTerraClim return
#' @export

getMACA = function(AOI, varname, timeRes = 'day',
                   model = 'CCSM4', scenario = 'rcp45', 
                   startDate, endDate = NULL,
                   verbose = FALSE, dryrun = FALSE){
  if(!timeRes %in% c('day', 'month')){ stop("timeRes must be month or day") }
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap(paste0('maca_', timeRes), test(as.list(match.call.defaults()[-1])), verbose, dryrun)
}

#' @title Get GridMet Climate Data for an Area of Interest
#' @description gridMET is a dataset of daily high-spatial resolution (~4-km, 1/24th degree) surface meteorological data covering the contiguous US from 1979-yesterday.
#' These data are updated daily.
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return
#' @export

getGridMET = function(AOI, varname, 
                      startDate, endDate = NULL,
                      verbose = FALSE, dryrun = FALSE){
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap("gridmet", test(as.list(match.call.defaults()[-1])), verbose, dryrun)
}


#' @title Get LOCA Climate Data for an Area of Interest
#' @description LOCA is a statistical downscaling technique that uses past history to add improved fine-scale detail to global climate models.
#' LOCA has been used to downscale 32 global climate models from the CMIP5 archive at a 1/16th degree spatial resolution, covering North America from central Mexico through Southern Canada. The historical period is 1950-2005,
#' and there are two future scenarios available: RCP 4.5 and RCP 8.5 over the period 2006-2100 (although some models stop in 2099). The variables currently available are daily minimum and maximum temperature, and daily precipitation.
#' For more information visit: http://loca.ucsd.edu/.
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return
#' @export

getLOCA = function(AOI, varname, 
                   model = 'CCSM4', scenario = 'rcp45', 
                   startDate, endDate = NULL, 
                   verbose = FALSE, dryrun = FALSE){
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap('loca', test(as.list(match.call.defaults()[-1])), verbose, dryrun, print.arg = TRUE)
}

#' @title Get VIC data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return
#' @export

getVIC = function(AOI, varname, 
                  model = 'CCSM4', scenario = 'rcp45', 
                  startDate, endDate = NULL,
                  verbose = FALSE, dryrun = FALSE){
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap('vic', test(as.list(match.call.defaults()[-1])), verbose, dryrun)
}

#' @title Get BCCA data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return
#' @export

getBCCA = function(AOI, varname, 
                   model = 'CCSM4', scenario = 'rcp45', ensemble = NULL, 
                   startDate, endDate = NULL,
                   verbose = FALSE, dryrun = FALSE){
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap('bcca', test(as.list(match.call.defaults()[-1])), verbose, dryrun)
}

#' @title Get CHIRPS data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return
#' @export

getCHIRPS = function(AOI, varname = NULL, 
                     period = "daily", 
                     startDate, endDate = NULL,
                     verbose = FALSE, dryrun = FALSE){
  
  period =  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(period), perl = TRUE)
  
  good_periods = c("Pentad", "Annual", "Daily", "Monthly")
  
  if(!period %in% good_periods){ stop("Period must be one of: ", paste(good_periods, collapse = ", "),  call. = FALSE) }
  
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap(paste0("chirps20Global", period, "P05"), test(as.list(match.call.defaults()[-1])), verbose, dryrun)
}

#' @title Get NLDAS data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return
#' @export

getNLDAS = function(AOI, varname = NULL,
                    model = NULL, 
                    startDate, endDate = NULL,
                    verbose = FALSE, dryrun = FALSE){
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap("NLDAS", test(as.list(match.call.defaults()[-1])), verbose, dryrun, print.arg = FALSE)
}

#' @title Get GLDAS data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return
#' @export

getGLDAS = function(AOI, varname = NULL, model = NULL, 
                    startDate, endDate = NULL,
                    verbose = FALSE, dryrun = FALSE){
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap("GLDAS", test(as.list(match.call.defaults()[-1])), verbose, dryrun)
}


#' @title Get MODIS data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @param asset The MODIS sensor
#' @inherit getTerraClim return
#' @export

getMODIS = function(AOI, asset = NULL, varname = NULL,
                    startDate, endDate = NULL,
                    verbose = FALSE, dryrun = FALSE){
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap("MODIS", test(as.list(match.call.defaults()[-1])), verbose, dryrun)

}

#' @title Get Livneh data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @param timeRes daily or monthly
#' @inherit getTerraClim return
#' @export

getLivneh = function(AOI, varname = NULL, 
                     startDate, endDate = NULL,  timeRes = "daily",
                     verbose = FALSE, dryrun = FALSE){
  if(timeRes == "daily"){
    test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
    climater_dap("Livneh_daily", test(as.list(match.call.defaults()[-1])), verbose, dryrun)
  } else {
    test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
    climater_dap("Livneh_monthly", test(as.list(match.call.defaults()[-1])), verbose, dryrun, print.arg = FALSE)
  }
}

#' @title Get Livneh Flux data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @param timeRes daily or monthly
#' @inherit getTerraClim return
#' @export

getLivneh_fluxes = function(AOI, varname = NULL, 
                            startDate, endDate = NULL,
                            verbose = FALSE, dryrun = FALSE){
  test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
  climater_dap("Livneh_fluxes", test(as.list(match.call.defaults()[-1])), verbose, dryrun)
}

#' @title Get PRISM data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @param timeRes daily or monthly
#' @inherit getTerraClim return
#' @export

getPRISM = function(AOI, varname = NULL, 
                    startDate, endDate = NULL, timeRes = "daily",
                    verbose = FALSE, dryrun = FALSE){
  
  x <- NULL
  
  if(timeRes == "daily"){
    
    raw = climater_filter(id = "prism_daily",
                          startDate = startDate, endDate = endDate,
                          varname = varname)
    
    if(is.null(endDate)){ endDate = startDate }
    
    d1 = seq.Date(as.Date(startDate), as.Date(endDate), by = 'day')
    d =  mutate(merge(d1, raw), YYYY = format(x, "%Y"),  YYYYMMDD = format(x, "%Y%m%d"))
    d$URL = glue(d$URL[1], YYYY = d$YYYY,  YYYYMMDD = d$YYYYMMDD)
    d$duration = paste0(d1, "/", d1)
    
    if(dryrun){
      dap_crop(catalog = d,  AOI = AOI, verbose = TRUE)
    } else {
      dap(catalog = d,  AOI = AOI, verbose = verbose)
    }
  } else {
    test = function(x){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }
    climater_dap(id = "prism_monthly",  test(as.list(match.call.defaults()[-1])), verbose, dryrun)
   }
}




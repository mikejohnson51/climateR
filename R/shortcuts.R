call_aoi = function(x, AOI){ if(x$AOI == "."){ x$AOI =  eval(parse(text=deparse(AOI)))}; x }

match.call.defaults <- function(...) {
  call <- evalq(match.call(expand.dots = FALSE), parent.frame(1))
  formals <- evalq(formals(), parent.frame(1))
  
  for(i in setdiff(names(formals), names(call)))
    call[i] <- list( formals[[i]] )
  
  match.call(sys.function(sys.parent()), call)
}

#' ClimateR dry run
#' @param id The resource name, agency, or catalog identifier
#' @param args The parent function arguments
#' @param verbose Should messages be emited?
#' @param dryrun Return summary of data prior to retrieving it
#' @param print.arg should arguments be printed? Usefull for debugging
#' @family dap
#' @return data.frame

climater_dap = function(id, args, verbose, dryrun, print.arg = FALSE){
  args$id = id
  if(print.arg){print(args)}
  args$catalog = do.call(climater_filter, args[names(args) %in% formalArgs(climater_filter)])
  args$verbose = verbose
  if(print.arg){print(args)}
  
  if(dryrun){
    args$verbose = TRUE
    args$varname = NULL
    if(print.arg){print(args)}
    x = do.call(dap_crop, args[names(args) %in% formalArgs(dap_crop)])
  } else {
    args$varname = NULL
    if(print.arg){print(args)}
    x = do.call(dap, args[names(args) %in% formalArgs(dap)])
  }
  
  x 
}

#' @title Get Terra Climate Data for an Area of Interest
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @param ID a column of unique identifiers
#' @return if AOI is polygon a list of SpatRasters, if AOI is a point then a data.frame of modeled records.
#' @family shortcuts
#' @export

getTerraClim = function(AOI, varname = NULL, 
                        startDate = NULL, endDate = NULL, 
                        verbose = FALSE, ID = NULL, dryrun = FALSE){
  climater_dap("terraclim",
               call_aoi(as.list(match.call.defaults()[-1]), AOI), 
               verbose, dryrun)
}

#' @title Get TerraClimate Normals for an Area of Interest
#' @description These layers from TerraClimate were creating using climatically aided interpolation of monthly anomalies from the CRU Ts4.0 and Japanese 55-year Reanalysis (JRA-55) datasets with WorldClim v2.0 climatologies.
#' @param month numeric. and month or vector of months to access. Default is 1:12
#' @inheritParams getTerraClim
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getTerraClimNormals = function(AOI, varname, 
                               scenario = '19812010' , month = 1:12, 
                               verbose = FALSE, ID = NULL, dryrun = FALSE){
  
  raw = climater_filter(id = "terraclim_normals", AOI = AOI, varname = varname, scenario = scenario)
  
  tmp = as.Date(strsplit(raw$duration[1], "/")[[1]])

  dates = seq.Date(tmp[1],tmp[2], by = "month", tz = "UTC")[month]
  
  if(dryrun) { 
    dap_crop(catalog = raw, AOI = AOI, startDate = head(dates, 1), endDate   = tail(dates, 1), verbose = TRUE)
  } else { 
    dap(catalog  = raw, AOI = AOI, startDate = head(dates, 1), endDate   = tail(dates, 1), ID = ID, verbose = verbose)
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
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getDaymet = function(AOI, varname = NULL, 
                     startDate = NULL, endDate = NULL, 
                     verbose = FALSE, ID = NULL, dryrun = FALSE){
  
  climater_dap("daymet4", call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
}

#' @title Get MACA Climate Data for an Area of Interest
#' @description Multivariate Adaptive Constructed Analogs (MACA) is a statistical method for downscaling Global Climate Models
#' (GCMs) from their native coarse resolution to a higher spatial resolution that captures reflects observed patterns of daily near-surface meteorology and simulated changes in GCMs experiments.
#' @param timeRes daily or monthly
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @param timeRes daily or monthly
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getMACA = function(AOI, varname, timeRes = 'day',
                   model = 'CCSM4', scenario = 'rcp45', 
                   startDate, endDate = NULL,
                   verbose = FALSE, ID = NULL, dryrun = FALSE){
  if(!timeRes %in% c('day', 'month')){ stop("timeRes must be month or day") }
  climater_dap(paste0('maca_', timeRes), call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
}

#' @title Get GridMet Climate Data for an Area of Interest
#' @description gridMET is a dataset of daily high-spatial resolution (~4-km, 1/24th degree) surface meteorological data covering the contiguous US from 1979-yesterday.
#' These data are updated daily.
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getGridMET = function(AOI, varname, 
                      startDate, endDate = NULL,
                      verbose = FALSE, ID = NULL, dryrun = FALSE){
  climater_dap("gridmet", call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
}


#' @title Get LOCA Climate Data for an Area of Interest
#' @description LOCA is a statistical downscaling technique that uses past history to add improved fine-scale detail to global climate models.
#' LOCA has been used to downscale 32 global climate models from the CMIP5 archive at a 1/16th degree spatial resolution, covering North America from central Mexico through Southern Canada. The historical period is 1950-2005,
#' and there are two future scenarios available: RCP 4.5 and RCP 8.5 over the period 2006-2100 (although some models stop in 2099). The variables currently available are daily minimum and maximum temperature, and daily precipitation.
#' For more information visit: http://loca.ucsd.edu/.
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getLOCA = function(AOI, varname, 
                   model = 'CCSM4', scenario = 'rcp45', 
                   startDate, endDate = NULL, 
                   verbose = FALSE, ID = NULL, dryrun = FALSE){
  climater_dap('loca', 
               call_aoi(as.list(match.call.defaults()[-1]), AOI), 
               verbose, dryrun)
}

#' @title Get VIC data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getVIC = function(AOI, varname, 
                  model = 'CCSM4', scenario = 'rcp45', 
                  startDate, endDate = NULL,
                  verbose = FALSE, ID = NULL, dryrun = FALSE){
  climater_dap('vic', call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
}

#' @title Get BCCA data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getBCCA = function(AOI, varname, 
                   model = 'CCSM4', scenario = 'rcp45', ensemble = NULL, 
                   startDate, endDate = NULL,
                   verbose = FALSE, ID = NULL, dryrun = FALSE){
  climater_dap('bcca', call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
}

#' @title Get CHIRPS data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @param timeRes "Pentad", "Annual", "Daily" (default), or "Monthly"
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getCHIRPS = function(AOI, varname = NULL, 
                     timeRes = "daily", 
                     startDate, endDate = NULL,
                     verbose = FALSE, ID = NULL, dryrun = FALSE){
  
  timeRes =  gsub("(^|[[:space:]])([[:alpha:]])", "\\1\\U\\2", tolower(timeRes), perl = TRUE)
  
  good_timeRes = c("Pentad", "Annual", "Daily", "Monthly")
  
  if(!timeRes %in% good_timeRes){ stop("timeRes must be one of: ", paste(good_timeRes, collapse = ", "),  call. = FALSE) }
  
  climater_dap(paste0("chirps20Global", timeRes, "P05"), 
               call_aoi(as.list(match.call.defaults()[-1]), AOI), 
               verbose, 
               dryrun)
}

#' @title Get NLDAS data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getNLDAS = function(AOI, varname = NULL,
                    model = 'FORA0125_H.002', 
                    startDate, endDate = NULL,
                    verbose = FALSE, ID = NULL, dryrun = FALSE){
  if(!checkNetrc()){
    stop('netrc file not found. Please run writeNetrc() with earth data credentials.')
  } else {
    x = writeDodsrc()
    data = climater_dap("NLDAS", call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)

    return(data)
  }
}

#' @title Get GLDAS data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getGLDAS = function(AOI, varname = NULL, model = NULL, 
                    startDate, endDate = NULL,
                    verbose = FALSE, ID = NULL, dryrun = FALSE){
  if(!checkNetrc()){
    stop('netrc file not found. Please run writeNetrc() with earth data credentials.')
  } else {
    x = writeDodsrc()
    data = climater_dap("GLDAS", call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
    unlink(x)
    return(data)
  }
}


#' @title Get MODIS data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @param asset The MODIS sensor
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getMODIS = function(AOI, asset = NULL, varname = NULL,
                    startDate, endDate = NULL,
                    verbose = FALSE, ID = NULL, dryrun = FALSE){
  if(!checkNetrc()){
    stop('netrc file not found. Please run writeNetrc() with earth data credentials.')
  } else {
    x = writeDodsrc()
    data = climater_dap("MODIS", call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
    unlink(x)
    return(data)
  }
  
}

#' @title Get Livneh data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @param timeRes daily or monthly
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getLivneh = function(AOI, varname = NULL, 
                     startDate, endDate = NULL,  timeRes = "daily",
                     verbose = FALSE, ID = NULL, dryrun = FALSE){
  if(timeRes == "daily"){
    climater_dap("Livneh_daily", call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
  } else {
    climater_dap("Livneh_monthly", call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
  }
}

#' @title Get Livneh Flux data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getLivneh_fluxes = function(AOI, varname = NULL, 
                            startDate, endDate = NULL,
                            verbose = FALSE, ID = NULL, dryrun = FALSE){
  climater_dap("Livneh_fluxes", call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
}

#' @title Get PRISM data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @param timeRes daily or monthly
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getPRISM = function(AOI, varname = NULL, 
                    startDate, endDate = NULL, timeRes = "daily",
                    verbose = FALSE, ID = NULL, dryrun = FALSE){
  
  x <- . <- NULL
  
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
      
      return(dap_crop(catalog = d,  AOI = AOI, verbose = TRUE))
      
    } else {
      
      o = lapply(1:nrow(d), function(x){
        dap(catalog = d[x,],  AOI = AOI, ID = ID, verbose = FALSE)
      }) 
    
      if(!inherits(o[[1]][[1]], "SpatRaster")){
        
        o = mutate(bind_rows(o), varname = d$varname)
        
        if(length(unique(o$varname)) > 1){
          x = lapply(split(o, varname), function(x) { x["varname"] <- NULL; x })
        } else {
          x = select(o, -varname)
        }
        
      } else {
        
        x = merge_across_time(unlist(o))
        
      }
    }
    
    return(x)
    
  } else {
    climater_dap(id = "prism_monthly",  call_aoi(as.list(match.call.defaults()[-1]), AOI), verbose, dryrun)
  }
}

#' @title Get LOCA Hydrology data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return 
#' @family shortcuts
#' @export

getLOCA_hydro = function(AOI, varname, 
                         model = 'CCSM4', scenario = 'rcp45', 
                         startDate, endDate = NULL, 
                         verbose = FALSE, ID = NULL, dryrun = FALSE){
  
  J <- year <- y <- URL <- layers <- nrows <- ncols <- n_values <- NULL
  
  
  x  = climater_filter(id = "loca_hydrology", 
                       AOI = AOI, varname = varname, 
                       model = model, scenario = scenario,
                       startDate = startDate, endDate = endDate)
  
  if(is.null(endDate)){ endDate = startDate}
  
  dap = data.frame(date = seq.Date(as.Date(startDate), as.Date(endDate), by = "day")) %>% 
    mutate(year = format(date, "%Y"),
           J = as.numeric(format(date, "%j"))) %>% 
    group_by(year) %>% 
    mutate(lyr1 = min(J), lyr2 = max(J),
           d1 = min(date), d2 = max(date)) %>% 
    slice(1) %>% 
    ungroup() %>% 
    merge(varname) %>%
    rename(varname = y) %>%
    left_join(x, by = "varname") %>% 
    mutate(URL = URL)
  
  if(dryrun){
    for(i in 1:nrow(dap)){
      r = rast(ext = make_ext(dap[i,]), crs = dap$crs[i], res = c(dap$resX[i], dap$resY[i])) %>% 
        crop(project(vect(AOI), dap$crs[i]))
      dap$URL[i]    = paste0('/vsicurl/', glue(dap$URL[i], year = dap$year[i]))
      dap$layers[i] = dap$lyr2[i] - dap$lyr1[i] + 1
      dap$nrows[i]  = nrow(r)
      dap$ncols[i]  = ncol(r)
      dap$n_values[i] = dap$layers[i] * dap$ncols[i] * dap$nrows[i]
    }
    
   return(select(dap, varname, URL, layers, nrows, ncols, n_values))
    
  } else {
    out = lapply(1:nrow(dap), function(x){ 
      read_ftp(cat   = dap[x, ], 
               URL   = paste0('/vsicurl/', glue(dap$URL[x], year = dap$year[x])), 
               lyrs  = c(dap$lyr1[x]:dap$lyr2[x]), 
               AOI   = AOI, 
               ext   = make_ext(dap[x,]), 
               crs   = dap$crs[x], 
               dates = seq.Date(dap$d1[x], dap$d2[x], by = dap$interval[x]))
    })

    names(out) = dap$varname
    
    merge_across_time(out)
  }
}

#' @title Get USGS 3DEP DEMs
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @param resolution DEM resolution (10m or 30m (default))
#' @inherit getTerraClim return 
#' @family shortcuts
#' @export

get3DEP = function(AOI, resolution = "30m", ID = NULL){
  
  description = NULL
  
  x = climater_filter(id = "USGS 3DEP") %>% 
    filter(grepl(resolution, description))
  
  return(dap(catalog = x, AOI = AOI, ID = ID))
  
}

#' @title Get NASA Global DEM
#' @inheritParams climater_filter
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getNASADEM = function(AOI, ID = NULL){
  return(dap(catalog = climater_filter(id = "NASADEM"), AOI = AOI, ID = ID))
}

#' @title Get USGS National Land Cover Dataset 
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @param year Landcover product year (2001, 2011,2016,2019)
#' @param type product type
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getNLCD = function(AOI, year = 2019, type = "land cover", ID = NULL){
  
  cat = climater_filter(id = "NLCD", AOI = AOI)
    if(!nrow(cat) > 0){ stop("No NLCD data in AOI.", call. = FALSE) }
  cat = cat[grepl(tolower(type), tolower(cat$description)), ]
    if(!nrow(cat) > 0){ stop("No ", type, " data in AOI.", call. = FALSE) }
  cat = cat[grepl(year, tolower(cat$description)), ]
    if(!nrow(cat) > 0){ stop("No ", type, " data in AOI for ", year, call. = FALSE) }
    
  return(dap(catalog = cat, AOI = AOI, ID = ID))
  
}

#' @title Get USGS LCMAP
#' @description Land Change Monitoring, Assessment, and Projection
#' @inheritParams getTerraClim
#' @param year Land cover product year 1985 - 2019 (default = 2019)
#' @param type product type (primary landcover (default), secondary landcover, primary confidence, secondary confidence, cover change, change day, change magniture, model cquality, spectral stability, spectral lastchance)
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getLCMAP = function(AOI, year = 2019, type = "primary landcover", ID = NULL){
  
  x = NULL
  
  cat = climater_filter(id = "LCMAP")
  cat = cat[grepl(year, tolower(cat$description)), ]
  if(!nrow(cat) > 0){ stop("No ", type, " data in AOI for ", year) }
  cat = cat[grepl(tolower(type), tolower(cat$description)), ]
  if(!nrow(cat) > 0){ stop("No ", type, " data in AOI.") }
 
  gid = sapply(1:nrow(cat), function(x) {
    suppressWarnings({
      tryCatch({
        sum(terra::is.related(terra::project(terra::ext(AOI), crs(AOI), cat$crs[x]),
                              make_vect(cat[x,]),
                              "intersects")) > 0
      }, error = function(e) {
        FALSE
      })
      
    })
  })
  
  cat = cat[gid, ]
  
  if(nrow(cat) == 0){
    stop("No data found in provided AOI.", call. = FALSE)
  } 
  
  return(dap(catalog = cat, AOI = AOI, ID = ID))
  
}


#' @title Get WorlClim gridded weather and climate data for historical (near current) conditions.
#' @description WorldClim is a database of high spatial resolution global weather and climate data. These data can be used for mapping and spatial modeling. 
#' @param month numeric. and month or vector of months to access. Default is 1:12
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getWorldClim = function(AOI = NULL, varname = NULL, model = "wc2.1_5m",
                        month = 1:12, ID = NULL, verbose = TRUE){
  
  description <- NULL
  raw = climater_filter(id = "WorldClim2.1", AOI = AOI, varname = varname, model = model) %>%
    filter(grepl(paste(month.name[month], collapse = "|"), description))
           
  if(nrow(raw) > 0){
    dap(catalog  = raw, AOI = AOI, verbose = verbose, ID = NULL)
  }
}

#' @title Get World Soil Information gridded weather and climate data for historical (near current) conditions.
#' @description World Soil Information (International Soil Reference and Information Centre) serves the international community with open access global soil data
#' @inheritParams climater_filter
#' @inheritParams climater_dap
#' @inheritParams getTerraClim
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

getISRIC_soils = function(AOI = NULL, varname = NULL, verbose = TRUE, ID = NULL){
  
  raw = climater_filter(id = "ISRIC Soil Grids", AOI = AOI, varname = varname) 
  
  if(nrow(raw) > 0){
    dap(catalog  = raw, AOI = AOI, verbose = verbose, ID = NULL)
  }
}

#' @title Get MERRA2
#' @description MERRA2
#' @inheritParams climater_filter
#' @param year Land cover product year 1985 - 2019 (default = 2019)
#' @param type product type (primary landcover (default), secondary landcover, primary confidence, secondary confidence, cover change, change day, change magniture, model cquality, spectral stability, spectral lastchance)
#' @inherit getTerraClim return
#' @family shortcuts
#' @export

# getMERRA2 = function(AOI, 
#                      varname, 
#                      startDate, 
#                      endDate = NULL){
#   
#   x = NULL
#   
#   cat = climater_filter(id = "MERRA2")
#   cat = cat[grepl(year, tolower(cat$description)), ]
#   if(!nrow(cat) > 0){ stop("No ", type, " data in AOI for ", year) }
#   cat = cat[grepl(tolower(type), tolower(cat$description)), ]
#   if(!nrow(cat) > 0){ stop("No ", type, " data in AOI.") }
#   
#   gid = sapply(1:nrow(cat), function(x) {
#     suppressWarnings({
#       tryCatch({
#         nrow(intersect(make_vect(cat = cat[x, ]), project(vect(AOI), crs(cat$crs[x])))) > 0
#       }, error = function(e) {
#         FALSE
#       })
#       
#     })
#   })
#   
#   cat = cat[gid, ]
#   
#   if(nrow(cat) == 0){
#     stop("No data found in provided AOI.", call. = FALSE)
#   } 
#   
#   return(dap(catalog = x, AOI = AOI))
#   
# }


getExtension = function(x) {
  pos <- regexpr("\\.([[:alnum:]]+)$", x)
  ifelse(pos > -1L, substring(x, pos + 1L), "")
}

#' Parse Dates from duration and interval
#' @param duration time duration
#' @param interval time interval
#' @family dap
#' @export

parse_date <- function(duration, interval) {
  
  d <- strsplit(duration, "/")[[1]]
  
  if (d[2] == "..") {
    d[2] <- as.character(Sys.Date())
  }
  
  if (interval == "1 month") {
    d[1] <- format(as.POSIXct(d[1], tz = "UTC"), "%Y-%m-01")
  } 
  
  if (grepl("hour", interval)) {
    d[1] <- format(as.POSIXct(d[1], tz = "UTC"), "%Y-%m-%d %H:%M:%S")
    d[2] <-
      format(as.POSIXct(d[2], tz = "UTC"), "%Y-%m-%d %H:%M:%S")
  }
  
   seq.POSIXt(as.POSIXct(d[1], tz = "UTC"),
             as.POSIXct(d[2], tz = "UTC"),
             interval)

  }

#' @title Get Data (Data Access Protocol)
#' @description this function provides a consistent data access protocol (dap) to a wide
#' range of local and remote resources including VRT, TDS, NetCDF
#' @description Define and get data from a DAP resource
#' @inheritParams climater_filter
#' @param grid a list containing an extent (), and crs
#' @param URL local file path or URL
#' @param catalog subset of open.dap catalog
#' @param start for non "dated" items, start can be called by index
#' @param end for non "dated" items, end can be called by index
#' @param toptobottom should data be inverse?
#' @param ID a column of unique identifiers
#' @param verbose  Should dap_summary be printed?
#' @details Wraps dap_get and dap_crop into one.
#' If AOI is NULL no spatial crop is executed. If startDate AND endDate are NULL, no temporal crop is executed. If just endDate is NULL it defaults to the startDate.
#' @return data.frame
#' @family dap
#' @export

dap <- function(URL = NULL,
                catalog = NULL,
                AOI = NULL,
                startDate = NULL,
                endDate = NULL,
                varname = NULL,
                grid    = NULL,
                start = NULL,
                end = NULL,
                toptobottom = FALSE,
                ID = NULL,
                verbose = TRUE) {
  
  urls = c(URL, catalog$URL)
  
  if(inherits(AOI, "list")){
    aoi = AOI[[1]]
  } else if(inherits(AOI, "sfc")){
    aoi = vect(AOI)
  } else {
    aoi = AOI
  }
  
  if (any(getExtension(urls) %in% c('vrt', "tif")) |  all(grepl("vsi", urls))) {
    x = vrt_crop_get(
      URL = URL,
      catalog = catalog,
      AOI = aoi,
      varname = varname,
      grid = grid,
      start = start,
      end = end,
      toptobottom = toptobottom,
      verbose = verbose
    )
   
  } else if(any(getExtension(urls) %in% 'zarr')) {
    
    zarr <- zarr_crop(
      URL = URL,
      catalog = catalog,
      AOI = aoi,
      startDate = startDate,
      endDate = endDate,
      start = start,
      end = end,
      varname = varname,
      verbose = verbose
    )
    
    x = zarr_get(zarr)
    
  } else {
    dap <- dap_crop(
      URL = URL,
      catalog = catalog,
      AOI = aoi,
      startDate = startDate,
      endDate = endDate,
      start = start,
      end = end,
      varname = varname,
      verbose = verbose
    )
    
    x = dap_get(dap)
    
  }
  
  if(!is.null(ID)){
    AOI = spatAOI(AOI)

    if(is.points(AOI)){
      x = extract_sites(x, AOI, ID)
    } else {
      type = toupper(geomtype(AOI))
      message(glue("We do not support summary to {type} yet. \n> Please see `install_github('mikejohnson51/zonal')"))
    }
  }
  
  if(class(AOI)[1] == "SpatRaster"){

    l = unlist(list(AOI,x), recursive = TRUE)
    
    names(l) =  c(names(AOI), names(x))
    
    l
  
  } else {
    x
  }

}

#' VRT Crop
#' @inheritParams dap
#' @return SpatRaster
#' @family dap
#' @export

vrt_crop_get = function(URL = NULL,
                        catalog = NULL,
                        AOI = NULL,
                        grid = NULL,
                        varname = NULL,
                        start = NULL,
                        end = NULL,
                        toptobottom = FALSE,
                        verbose = TRUE) {
  
  if (is.null(URL)) {
    URL <- catalog$URL
  }
  
  vrts = tryCatch({
    suppressWarnings({ rast(URL) })
  }, error = function(e){
    tryCatch({
      suppressWarnings({ rast(URL) })
    }, error = function(e){
      NULL
    })
  })
  
  if(!is.null(vrts)){
  
  if (!is.null(varname)) {
    vrts = vrts[[grepl(paste(varname, collapse = "|"), names(vrts))]]
  }
  
  if (!is.null(start) & is.null(end)) { vrts  = vrts[[start]] }
  if (!is.null(start) & !is.null(end)) {  vrts = vrts[[start:end]] }
  
  if (!is.null(grid)) {
    ext(vrts) <- grid$extent
    crs(vrts) <- grid$crs
    fin = vrts
    flag = TRUE
  } else {
    if (crs(vrts[[1]]) == "" | all(as.vector(ext(vrts[[1]])) == c(0, 1, 0, 1))) {
      if (verbose) {
        warning("Defined URL(s) are aspatial and on a unit grid. Cannot be cropped",
                call. = FALSE)
      }
      
      flag = FALSE
    }
    
    fin = vrts
    flag = TRUE
  }
    
  } else {
    flag = TRUE
    fin = suppressWarnings({ lapply(URL, rast) })
  }
  
  if (!is.null(AOI) & flag) {
    fin = tryCatch({
      crop(fin, project(spatAOI(AOI), crs(fin[[1]])))
    }, error = function(e) {
      lapply(1:length(fin), function(x) {
        crop(fin[[x]], project(spatAOI(AOI), crs(fin[[x]])))
      })
    })
  }
  
  if (toptobottom) {
    fin <- lapply(fin, flip)[[1]]
  } else {
    fin <-  fin
  }
  
  names(fin) = if(!is.null(catalog) & nlyr(fin) == 1){
    catalog$asset
  } else {
    as.vector(sapply(fin, names))
  }
  
  fin

}

#' @title Crop DAP file
#' @inheritParams dap
#' @return data.frame
#' @family dap
#' @export

dap_crop <- function(URL = NULL,
                     catalog = NULL,
                     AOI = NULL,
                     startDate = NULL,
                     endDate = NULL,
                     start  = NULL,
                     end = NULL,
                     varname = NULL,
                     verbose = TRUE) {
  
  interval <- NULL
  
  if (!is.null(URL)) {
    catalog <- read_dap_file(URL, varname = varname, id = "local")
    catalog$tiled <- ""
  }
  
  ## TIME
  
  for(i in 1:nrow(catalog)){
    if(grepl("[..]", catalog$duration[i])){
      tmp = .resource_time(URL = catalog$URL[i], T_name = catalog$T_name[i])
      catalog$duration[i] = tmp$duration
      catalog$interval[i] = tmp$interval
      catalog$interval[i] = tmp$interval
    }
  }
  
  if (is.null(startDate) & is.null(endDate)) {
    
    if(is.null(start) & is.null(end)){
      catalog$T <- paste0("[0:1:", catalog$nT - 1, "]")
      catalog$Tdim <- catalog$nT
      tmp <- do.call(rbind, strsplit(catalog$duration, "/"))
      catalog <- cbind(catalog, data.frame(startDate = tmp[, 1], endDate = tmp[, 2]))
    } else {
      if(is.null(end)){ end = start}
      catalog$T <- paste0("[", start - 1,  ":1:", end - 1, "]")
      catalog$Tdim <- max(end - start, 1)
      
      for(i in 1:nrow(catalog)){
        tmp <- strsplit(catalog$duration[i], "/")[[1]]
        d = seq.Date(as.Date(tmp[1]), as.Date(tmp[2]), by = catalog$interval[1])
        catalog$startDate = d[start]
        catalog$endDate = d[end]
      }
    }
    
  } else {
    
    if (is.null(endDate)) { endDate <- startDate }
    
    if (grepl("hour", catalog$interval[1])) {
      startDate <- paste(startDate, "00:00:00")
      endDate   <- paste(endDate,   "23:00:00")
    }
    
    startDate <- as.POSIXct(as.character(startDate), tz = "UTC")
    endDate   <-  as.POSIXct(as.character(endDate), tz = "UTC")
    
    out <- list()
    
    catalog = catalog %>%
      mutate(interval = ifelse(interval == "monthly normal", "month", interval))
  
    for (i in 1:nrow(catalog)) {
      
      time_steps <- parse_date(duration = catalog$duration[i], interval = catalog$interval[i])
      
      int_unit = strsplit(catalog$interval[i], " ")[[1]][2]
      
      time_steps = trunc(time_steps, int_unit)
      
      if(catalog$nT[i] == 1 & !is.na(catalog$nT[i])){
        out[[i]] <- cbind(
          catalog[i,],
          data.frame(
            T = "[0:1:0]",
            Tdim = 1,
            startDate = time_steps[1],
            endDate = time_steps[1]
          )
        )
      } else if (startDate > max(time_steps) |  endDate  < min(time_steps)) {
        out[[i]] <- NULL
      } else {
        tmp = abs(time_steps - startDate)
        # upper limit on tied lower threshold
        T1 <- max(which(tmp == min(tmp)))
        # 
        if(startDate == endDate){
          Tn = T1
        } else {
          tmp = abs(time_steps - endDate)
          # lower limit on tied upper threshold
          Tn <-  min(which(tmp == min(tmp)))
        }
       
        out[[i]] <- cbind(
          catalog[i,],
          data.frame(
            T = paste0("[", T1 - 1, ":1:", Tn - 1, "]"),
            Tdim = (Tn - T1) + 1,
            startDate = time_steps[T1],
            endDate = time_steps[Tn]
          )
        )
      }
    }
    
    catalog <- do.call(rbind, out)
    
    if (length(catalog) == 0) {
      stop("Requested Time not found in ",
           unique(catalog$duration),
           call. = FALSE)
    }
  }
  
  
  ## SPACE (XY)
  if (is.null(AOI)) {
    catalog$X <- paste0("[0:1:", catalog$ncols - 1, "]")
    catalog$Y <- paste0("[0:1:", catalog$nrows - 1, "]")
  } else {
    
    out <- lapply(1:nrow(catalog), function(i) {
      if(is.na(catalog$crs[i])){
        warning("No assigned CRS. Trying WGS84")
        catalog$crs[i] = "EPSG:4326"
      }
      tryCatch({
        ext(terra::intersect(terra::project(terra::ext(AOI), crs(AOI), catalog$crs[i]), 
                             make_vect(catalog[i,])))
      },
      error = function(e) {
        NULL
      })
    })
    
    drops <- which(sapply(out, is.null))
    
    if (length(drops) != 0) {
      catalog <- catalog[-drops,]
      out <- catalog[-drops]
    }
    
    if (nrow(catalog) < 1) {
      stop("No resources intersect with provided AOI", call. = FALSE)
    }
    
    for (i in 1:nrow(catalog)) {
      X_coords <-
        seq(catalog$X1[i], catalog$Xn[i], length.out = catalog$ncols[i])
      
      Y_coords <-
        seq(catalog$Y1[i], catalog$Yn[i], length.out = catalog$nrows[i])
      
      ys <-
        c(which.min(abs(Y_coords - out[[i]]$ymin)), which.min(abs(Y_coords - out[[i]]$ymax))) - 1
      xs <-
        c(which.min(abs(X_coords - out[[i]]$xmin)), which.min(abs(X_coords - out[[i]]$xmax))) - 1
      
      catalog$Y[i] <-
        paste0("[", paste(sort(ys), collapse = ":1:"), "]")
      catalog$X[i] <-
        paste0("[", paste(sort(xs), collapse = ":1:"), "]")
      catalog$X1[i] <- min(X_coords[xs + 1])
      catalog$Xn[i] <- max(X_coords[xs + 1])
      catalog$Y1[i] <- min(Y_coords[ys + 1])
      catalog$Yn[i] <- max(Y_coords[ys + 1])
      catalog$ncols[i] <- abs(diff(xs)) + 1
      catalog$nrows[i] <- abs(diff(ys)) + 1
      catalog
    }
    
  }
  
  first  = substr(catalog$dim_order,  1, 1)
  second = substr(catalog$dim_order, 2, 2)
  third  = substr(catalog$dim_order,  3, 3)
  
  first = ifelse(length(first) == 0, "T", first)
  second = ifelse(length(second) == 0, "Y", second)
  third = ifelse(length(third) == 0, "X", third)
  
  if (any(grepl("XY", catalog$tiled))) {
    catalog$URL <-
      glue(
        "{catalog$URL}?{catalog$varname}{catalog[[first]]}{catalog[[second]]}{catalog[[third]]}"
      )
  } else {
    catalog$URL <-
      glue(
        "{catalog$URL}?{catalog$varname}{catalog[[first]]}{catalog[[second]]}{catalog[[third]]}"
      )
  }
  
  if (!is.null(varname)) {
    if (all(!varname %in% catalog$varname, !varname %in% catalog$variable)) {
      stop(
        "variable(s) in resource include:\n\t> ",
        paste(unique(catalog$varname), collapse = "\t >"),
        call. = FALSE
      )
    }
    
    catalog <- catalog[catalog$varname %in% varname,]
  }
  
  catalog$X <- NULL
  catalog$Y <- NULL
  catalog$T <- NULL
  
  if (verbose) {
    dap_summary(catalog)
  }
  
  catalog
}

#' Get DAP resource data
#' @param dap data.frame from catalog or dap_crop
#' @param varname  name of variable to extract. If NULL, then get all
#' @return SpatRaster
#' @family dap
#' @export

dap_get <- function(dap, varname = NULL) {
  
  if (!is.null(varname)) {
    if (any(!varname %in% dap$varname | !varname %in% dap$variable)) {
      stop(
        "variable(s) in resource include:\n\t> ",
        paste(unique(dap$varname),
              collapse = "\t >"),
        call. = FALSE
      )
    }
    
    dap <- dap[dap$varname %in% varname | dap$variable %in% varname,]
  }
  
  out <- future_lapply(
    1:nrow(dap),
    FUN = function(x) {
      go_get_dap_data(dap = dap[x,])
    }
  )
  
  names(out) <- sub("_$", "", paste0(dap$varname))
  
  if(!inherits(out[[1]], "SpatRaster")){
   Reduce(function(dtf1, dtf2) { merge(dtf1, dtf2, by = "date", all.x = TRUE) }, out)
    
  } else if (any(grepl("XY", dap$tiled))) {
    ll = list()
    u <- unique(unlist(lapply(out, units)))
    if (length(u) == 1) {
      out <- suppressWarnings({
        merge(sprc(out))
      })
      units(out) <- rep(u, nlyr(out))
      out
    } else {
      out
    }
    
    ll[[dap$varname[1]]] = out
    ll
  } else if (any(dap$tiled == "T")) {
    merge_across_time(out)
  } else {
    out
  }
}

#' Print Summary Information About a OpenDAP Resource
#' @description Print summary information about a DAP summary
#' @param dap data.frame from catalog or dap_crop
#' @param url Unique Resource Identifier (http or local)
#' @family dap
#' @export

dap_summary <- function(dap = NULL, url = NULL) {
  
  if (!is.null(url) & is.null(dap)) {
    dap <- dap_crop(url)
  } else {
    resx <- (dap$Xn - dap$X1) / (dap$ncols - 1)
    resx = ifelse(is.na(resx), 'POINT', resx)
    resy <- (dap$Yn - dap$Y1) / (dap$nrows - 1)
    resy = ifelse(is.na(resy), 'POINT', resx)
    
    if (all(c(dap$nrows, dap$ncols) == 1)) {
      xmin <- dap$X1
      xmax <- dap$X1
      ymin <- dap$Y1
      ymax <- dap$Y1
      ncol <- 1
      nrow <- 1
    } else {
      xmin <- min(dap$X1 - 0.5 * resx)
      xmax <- max(dap$Xn + 0.5 * resx)
      ymin <- min(dap$Y1 - 0.5 * resy)
      ymax <- max(dap$Yn + 0.5 * resy)
      ncol <- round((xmax - xmin) / unique(resy)[1])
      nrow <- round((ymax - ymin) / unique(resx)[1])
    }
    
    ext <- paste0(paste(round(c(
      xmin, xmax, ymin, ymax
    ), 2), collapse = ", "),
    " (xmin, xmax, ymin, ymax)")
    
    
    minDate <- min(as.POSIXct(dap$startDate))
    
    maxDate <- max(as.POSIXct(dap$endDate))
    
    if (dap$interval[1] != 0) {
      tDim <- length(seq.POSIXt(minDate, maxDate, by = dap$interval[1]))
    } else {
      tDim = 1
    }
    
    var <-
      unique(paste0(
        dap$varname,
        " [",
        dap$units,
        "] (",
        ifelse(is.na(dap$description), "-", dap$description),
        ")"
      ))
    
    a <- dap$crs[1]
    b <- strsplit(dap$URL[1], "\\?")[[1]][1]
    b <- ifelse(nchar(b) > 60, paste0(strtrim(b, 60), "..."), b)
    
    {
      cat("source:\t", b, "\n")
      if (max(table(dap$varname)) > 1) {
        cat("tiles:\t", max(table(dap$varname)), unique(dap$tiled), "tiles\n")
      }
      cat("varname(s):\n  ", paste(">", var, collapse = "\n   "))
      cat(paste0("\n", paste(rep("=", 50), collapse = "")))
      cat(
        "\ndiminsions: ",
        paste0(
          round(ncol),
          ", ",
          round(nrow),
          ", ",
          tDim,
          " (names: ",
          dap$X_name[1],
          ",",
          dap$Y_name[1],
          ",",
          dap$T_name[1],
          ")"
        )
      )
      cat("\nresolution: ",
          paste0(
            round(dap$resX[1], 3),
            ", ",
            round(dap$resY[1], 3),
            ", ",
            max(1, dap$interval[1])
          ))
      cat("\nextent:     ", ext)
      cat("\ncrs:        ", ifelse(nchar(a) > 50, paste0(strtrim(a, 50), "..."), a))
      cat("\ntime:       ",
          as.character(minDate),
          "to",
          as.character(maxDate))
          cat(paste0("\n", paste(rep("=", 50), collapse = "")))
          cat(
            "\nvalues:",
            formatC(
              nrow * ncol * tDim * length(var),
              big.mark = ",",
              digits = 0,
              format = "f"
            ),
            "(vars*X*Y*T)"
          )
          }
    }
}


#' Read from FTP
#' @param URL Unique Resource Identifier (http or local)
#' @param cat catalog element
#' @param lyrs lyrs to extract
#' @param AOI Area of Interest
#' @param ext extent of source (if needed)
#' @param crs crs of source (if needed) 
#' @param dates dates of data
#' @return SpatRaster
#' @family dap
#' @export

read_ftp = function(URL, cat, lyrs = 1, AOI, ext = NULL, crs = NULL, dates = NULL){
  
  o = suppressWarnings(rast(URL , lyrs = lyrs))
  
  
  if(!is.null(crs)){
    crs(o) = crs
  }
  
  if(!is.null(ext)){
    ext(o) = ext
  }
  
  e =  align(ext(project(spatAOI(AOI), crs(o))), o)
  
  if(cat$toptobottom){
    z = crop(o, e)
  } else {
    
    e =  align(ext(project(spatAOI(AOI), crs(o))), o)
    
    ymax <- ymax(o) - (e$ymin - ymin(o))
    ymin <- ymax(o) - (e$ymax - ymin(o))
    
    flipe <- ext(c(xmin(e), xmax(e), as.numeric(ymin),  as.numeric(ymax)))
    
    z <- flip(crop(o, flipe), "vertical")
    ext(z) <- e
  }
  
  if(!is.null(dates)){
    time(z) = dates
  }
  
  names(z) <-  gsub("_NA", "",paste(cat$variable, 
                                    dates,
                                    cat$model, 
                                    cat$ensemble, 
                                    cat$scenario,
                                    sep = "_"))
  z
}


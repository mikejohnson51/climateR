#' Read Zarr File
#'
#' Reads a Zarr file from a specified URL and extracts metadata and variable information.
#'
#' @param URL Character. The URL of the Zarr file.
#' @param varname Character. Variable name to extract. Defaults to NULL.
#' @param id Character. An identifier for the dataset.
#' @param varmeta Logical. Whether to include variable metadata. Defaults to TRUE.
#' @return A data frame with merged metadata and variable information.
#' @family zarr
#' @export

read_zarr_file <- function(URL, varname = NULL, id, varmeta = TRUE) {
  
  nz <- rnz::open_nz(URL)
  on.exit(rnz::close_nz(nz))
  
  raw <- zarr_xyzv(obj = nz, varname, varmeta = varmeta)
  raw$URL <- URL
  raw$id <- id
  
  raw <- merge(raw, data.frame(.resource_time_zarr(URL, T_name = raw$T_name[1]), id = id), by = "id")
  
  raw <- merge(raw, .resource_grid_zarr(URL, X_name = raw$X_name[1], Y_name = raw$Y_name[1]))
  
  raw
}

#' Extract Variable Information from Zarr Object
#'
#' Extracts variable and coordinate information from a Zarr object.
#'
#' @param obj Zarr object or file path.
#' @param varname Character. Specific variable name to extract. Defaults to NULL.
#' @param varmeta Logical. Whether to include variable metadata. Defaults to FALSE.
#' @return A data frame containing variable metadata and coordinate information.
#' @family zarr
#' @export

zarr_xyzv <- function(obj, varname = NULL, varmeta = FALSE) {
  
  if (!inherits(obj, "ZarrGroup")) {
    obj <- rnz::open_nz(obj)
    on.exit(rnz::close_nz(obj))
  }
  
  
  if(is.null(varname)){
    raw = suppressWarnings({
      tryCatch({
        nc_coord_var(obj, variable = NULL)[, c("variable", "X", "Y", "T", "Z")]
      }, error = function(e){ 
        vars = nc_vars(obj)$name
        lapply(1:length(vars), FUN = function(j){ 
          tryCatch({
            nc_coord_var(obj, variable = vars[j])[, c("variable", "X", "Y", "T", "Z")]
          },
          error = function(x){
            NULL
          })
        }) 
      }) %>% 
        bind_rows() 
    })
    
  } else {
    
    raw = suppressWarnings({
      tryCatch({
        nc_coord_var(obj, variable = varname)[, c("variable", "X", "Y", "T", "Z")]
      }, error = function(e){ 
        NULL
      }) 
    })
  }
  
  
  raw <- raw[!apply(raw, 1, function(x) {
    sum(!is.na(x)) <= 3
  }), ]
  
  
  raw$dim_order = NA
  raw$nX = NA
  raw$nY = NA
  raw$nZ = NA
  
  
  for(i in 1:nrow(raw)){
    
    o      = rnz::inq_var(obj, raw$variable[1])$dimids
    dims   = nc_dims(obj, varname)$name[o + 1]
    length = nc_dims(obj, varname)$length[o + 1]
    
    if(is.na(raw$Z[i]) & length(dims) == 4){
      raw$Z[i] =  dims[!dims %in% as.vector(raw[i,-1])]
    }
    
    o = names(raw)[match(dims, raw[i,])]
    raw$dim_order[i] = paste(o, collapse = "")
    raw$nX[i] = length[which(dims == raw$X[i])]
    raw$nY[i] = length[which(dims == raw$Y[i])]
    #raw$nT[i] = length[which(dims == raw$T[i])]
    if(is.na(raw$Z)[i]){
      raw$nZ[i] = NA
    } else {
      raw$nZ[i] = length[which(dims == raw$Z[i])]
    }
  }
  
  names(raw) <- c("varname", "X_name", "Y_name", "T_name", "Z_name", "dim_order",
                  "nX", "nY", "nZ")
  
  ll <- list()
  
  if (varmeta) {
    for (i in 1:nrow(raw)) {
      if (unique(nc_var(obj, raw$varname[i])$ndims) > 4) {
        ll[[i]] <- NULL
        warning("We do not support 5D datasets:", raw$varname[i])
      } else {
        ll[[i]] <- data.frame(
          varname = raw$varname[i],
          units = try_att(obj, raw$varname[i], "units"),
          description = try_att(obj, raw$varname[i], "long_name")
        )
        
        if(is.na(ll[[i]]$description)){
          ll[[i]]$description = try_att(obj, raw$varname[i], "LongName")
        }
        
        ll[[i]]$description = gsub("\\s+"," ", ll[[i]]$description)
      }
    }
    
    merge(raw, do.call(rbind, ll), by = "varname")
  } else {
    raw
  }
}

#' Extract Time Information from Zarr Resource
#'
#' Retrieves time dimension metadata and calculates time intervals.
#'
#' @param URL Character. The URL of the Zarr file.
#' @param T_name Character. Name of the time variable. Defaults to NULL.
#' @return A list with time duration, interval, and count information.
#' @family zarr
#' @export

.resource_time_zarr <- function(URL, T_name = NULL) {
  
  if (is.null(T_name)) {
    nz = rnz::open_nz(URL)
    atts <- zarr_xyzv(nz)
    T_name <- omit.na(unique(atts$T_name))
    rnz::close_nz(nz)
  }
  
  nz = rnz::open_nz(URL)
    
  time_steps <- utcal.nc(
      unitstring =  rnz::get_att(nz, T_name, "units"),
      value = rnz::get_var(nz, T_name, unpack = TRUE),
      type = "c")
  
  dT <- diff(time_steps)
  
  g <- data.frame(expand.grid(unique(dT), units(dT)))
  g <- g[order(g$Var1), ]
  g$n <- as.numeric(table(dT))
  
  names(g) <- c("value", "interval", "n")
  
  if (nrow(g) > 1 & all(g$value %in% c(28, 29, 30, 31))) {
    g <- data.frame(value = 1, interval = "months")
  } else {
    g <- g[which.max(g$n), ]
  }
  
  # If time is within 5 days of today then we call the range Open
  maxDate <- ifelse(max(time_steps) >= Sys.time() - (5 * 86400) & max(time_steps) <= Sys.time() + 1,
                    "..",
                    as.character(max(time_steps))
  )
  
  nT <- ifelse(maxDate == "..", NA, length(time_steps))
  
  int <- paste(g$value, g$interval)
  
  if (length(int) == 0) {
    int <- "0"
  }
  
  list(
    duration = paste0(min(time_steps), "/", maxDate),
    interval = int,
    nT = nT
  )
}

#' Extract Grid Information from Zarr Resource
#'
#' Retrieves grid dimension metadata and calculates grid properties.
#'
#' @param URL Character. The URL of the Zarr file.
#' @param X_name Character. Name of the X-coordinate variable. Defaults to NULL.
#' @param Y_name Character. Name of the Y-coordinate variable. Defaults to NULL.
#' @param stopIfNotEqualSpaced Logical. Whether to stop if grid cells are not equally spaced. Defaults to TRUE.
#' @return A data frame with grid properties.
#' @family zarr
#' @export

.resource_grid_zarr <- function(URL, X_name = NULL, Y_name = NULL, stopIfNotEqualSpaced = TRUE) {
  
  nz <- rnz::open_nz(URL)
  
  if (is.null(X_name) | is.null(Y_name)) {
    atts <- zarr_xyzv(nz)
    X_name <- omit.na(unique(atts$X_name))
    Y_name <- omit.na(unique(atts$Y_name))
  }
  
  nc_grid_mapping <- suppressWarnings(nc_grid_mapping_atts(nz))
  
  degree <- grepl("degree", try_att(nz, X_name, "units"), ignore.case = TRUE)
  
  if (nrow(nc_grid_mapping) == 0) {
    if (degree) {
      message(paste(
        "No projection information found. \n",
        "Coordinate variable units are degrees so, \n",
        "assuming EPSG:4326"
      ))
      crs <- "EPSG:4326"
    } else {
      warning("No projection information found in nc file.")
      crs <- NA
    }
  } else {
    crs <- try(nc_gm_to_prj(nc_grid_mapping))
    if (inherits(crs, "try-error")) {
      crs <- NA
    } else {
      crs
    }
  }
  
  ncols <-  rnz::inq_dim(nz, X_name)$len
  nrows <-  rnz::inq_dim(nz, Y_name)$len
  
  xx <- try(rnz::get_var(nz, X_name))
  
  if (inherits(xx, "try-error")) { xx <- seq_len(ncols) }
  
  rs <- xx[-length(xx)] - xx[-1]
  
  if (!isTRUE(all.equal(min(rs), max(rs), tolerance = 0.025, scale = abs(min(rs))))) {
    if (is.na(stopIfNotEqualSpaced)) {
      warning("cells are not equally spaced; you should extract values as points")
    } else if (stopIfNotEqualSpaced) {
      stop("cells are not equally spaced; you should extract values as points")
    }
  }
  
  if (any(xx > 180) & degree) {  xx <- xx - 360 }
  
  xrange <- c(min(xx), max(xx))
  resx <- (xrange[2] - xrange[1]) / (ncols - 1)
  X1 <- xx[1]
  Xn <- xx[length(xx)]

  yy <- try(rnz::get_var(nz, Y_name))
    
  if (inherits(yy, "try-error")) { yy <- seq_len(nrows) }
  
  Y1 <- yy[1]
  Yn <- yy[length(yy)]
  
  rs <- yy[-length(yy)] - yy[-1]
  
  if (!isTRUE(all.equal(min(rs), max(rs), tolerance = 0.025, scale = abs(min(rs))))) {
    if (is.na(stopIfNotEqualSpaced)) {
      warning("cells are not equally spaced; you should extract values as points")
    } else if (stopIfNotEqualSpaced) {
      stop("cells are not equally spaced; you should extract values as points")
    }
  }
  
  yrange <- c(min(yy), max(yy))
  resy <- (yrange[2] - yrange[1]) / (nrows - 1)
  
  if (yy[1] > yy[length(yy)]) {
    toptobottom <- FALSE
  } else {
    toptobottom <- TRUE
  }
  
  data.frame(
    crs = crs,
    # xmin, xmax, ymin, ymax
    X1 = X1,
    Xn = Xn,
    Y1 = Y1,
    Yn = Yn,
    resX = resx,
    resY = resy,
    ncols = ncols,
    nrows = nrows,
    toptobottom = toptobottom
  )
}


#' Crop Zarr Data to a Spatial and Temporal Subset
#'
#' Crops data in a Zarr file based on spatial (AOI) and temporal (start/end) filters.
#'
#' @param URL Character. The URL of the Zarr file. Defaults to NULL.
#' @param catalog Data frame. Metadata catalog for the Zarr file. Defaults to NULL.
#' @param AOI Spatial object. Area of interest for cropping. Defaults to NULL.
#' @param startDate Character. Start date for cropping. Defaults to NULL.
#' @param endDate Character. End date for cropping. Defaults to NULL.
#' @param start Numeric. Start index for cropping. Defaults to NULL.
#' @param end Numeric. End index for cropping. Defaults to NULL.
#' @param varname Character. Variable name to crop. Defaults to NULL.
#' @param verbose Logical. Whether to print verbose output. Defaults to TRUE.
#' @return A cropped dataset matching the specified criteria.
#' @family zarr
#' @export

zarr_crop <- function(URL = NULL,
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
    catalog <- read_zarr_file(URL, varname = varname, id = "local")
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
      
      catalog$Y[i] <-  paste0("[", paste(sort(ys), collapse = ":1:"), "]")
      catalog$X[i] <-  paste0("[", paste(sort(xs), collapse = ":1:"), "]")
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
  
  catalog$variable = catalog$varname
  
  if (verbose) {
    dap_summary(catalog)
  }
  
  catalog
}

#' Retrieve data from a Zarr resource
#'
#' This function retrieves or prepares metadata for data stored in a Zarr format. 
#'
#' @param zarr A data frame containing details of the Zarr resource to process. 
#' Each row should correspond to a single Zarr resource.
#' @param get Logical. If `TRUE`, retrieves data; if `FALSE`, returns metadata information.
#'
#' @return If `get = TRUE`, returns the requested data as a matrix. If `get = FALSE`, 
#' returns a data frame containing metadata information for the resource.
#' @examples
#' \dontrun{
#' # Example usage (assuming `zarr` is a properly formatted data frame):
#' # result <- go_get_zarr(zarr, get = TRUE)
#' }
#' @family zarr
#' @export

go_get_zarr = function(zarr, get = TRUE) {
  
  if (nrow(zarr) != 1) {
    stop("This function processes only 1 zarr row at a time ... currently there are ", nrow(zarr))
  }
  
  nz <- rnz::open_nz(sub("\\?.*", "", zarr$URL))
  on.exit(rnz::close_nz(nz))
  # 
  k <- regmatches(zarr$URL, gregexpr("\\[.*?\\]", zarr$URL))[[1]]
  k <- gsub("[", "", k, fixed = TRUE)
  k <- gsub("]", "", k, fixed = TRUE)

  nz_var_info <- rnz::inq_var(nz, zarr$varname)
  X_var_info  <- rnz::inq_var(nz, zarr$X_name)$dimids
  Y_var_info  <- rnz::inq_var(nz, zarr$Y_name)$dimids
  T_var_info  <- rnz::inq_var(nz, zarr$T_name)$dimids
  
  o = if(zarr$dim_order == "XYT") {
    c(zarr$X1, zarr$Y1, zarr$T)
    c(X_var_info, Y_var_info, T_var_info)
  } else if ( zarr$dim_order == "XTY" ){
    c(X_var_info, T_var_info, Y_var_info)
  } else if ( zarr$dim_order == "YXT" ){
    c(Y_var_info, X_var_info, T_var_info)
  } else if ( zarr$dim_order == "YTX" ){
    c(Y_var_info, T_var_info, X_var_info)
  } else if ( zarr$dim_order == "TXY" ){
    c(T_var_info, X_var_info, Y_var_info)
  } else if ( zarr$dim_order == "TYX" ){
    c(T_var_info, Y_var_info, X_var_info)
  } 

  dimid_order <- match(
    nz_var_info$dimids,
    o
  )
  
  start <- (as.numeric(sapply(strsplit(k, ":"), "[[", 1)) + 1)[dimid_order]
  
  count <- (c(zarr$Tdim, zarr$nrows, zarr$ncols))[dimid_order]
  
  if (get) {
    rnz::get_var(nz,
                  zarr$varname,
                  start = start,
                  count = count,
                  unpack = TRUE) 
  } else {
    data.frame(
      file = sub("\\?.*", "", zarr$URL),
      variable = zarr$varname,
      start = I(list(start)),
      count = I(list(count)), 
      unpack = TRUE
    )
  }
}


#' Process and retrieve Zarr data as terra objects
#'
#' This function processes Zarr resources and converts them into terra spatial objects.
#'
#' @param zarr A data frame containing details of the Zarr resources to process.
#' Each row should correspond to a single Zarr resource.
#' @param varname Character vector specifying the variable names to retrieve. 
#' If `NULL`, retrieves all available variables.
#'
#' @return Returns processed data as terra SpatRaster objects or merged data frames, 
#' depending on the input and Zarr properties.
#' @examples
#' \dontrun{
#' # Example usage (assuming `zarr` is a properly formatted data frame):
#' # result <- zarr_get(zarr, varname = "temperature")
#' }
#' @family zarr
#' @export

zarr_get <- function(zarr, varname = NULL) {
  
  if (!is.null(varname)) {
    if (all(!varname %in% zarr$varname, !varname %in% zarr$variable)) {
      stop(
        "variable(s) in resource include:\n\t> ",
        paste(unique(zarr$varname),
              collapse = "\t >"),
        call. = FALSE
      )
    }
    
    zarr <- zarr[zarr$varname %in% varname | zarr$variable %in% varname,]
  }
  
  out <- future_lapply(
    1:nrow(zarr),
    FUN = function(x) {
      zarr_to_terra(go_get_zarr(zarr = zarr[x,]), zarr = zarr[x,])
    }
  )
  
  names(out) <- sub("_$", "", paste0(zarr$varname))
  
  if(!inherits(out[[1]], "SpatRaster")){
    Reduce(function(dtf1, dtf2) { merge(dtf1, dtf2, by = "date", all.x = TRUE) }, out)
    
  } else if (any(grepl("XY", zarr$tiled))) {
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
    
    ll[[zarr$varname[1]]] = out
    ll
  } else if (any(zarr$tiled == "T")) {
    merge_across_time(out)
  } else {
    out
  }
}

#' Convert data to terra SpatRaster or data frame format
#'
#' Converts extracted Zarr data into terra SpatRaster objects or data frames based on 
#' spatial and temporal dimensions.
#'
#' @param var A variable containing extracted Zarr data.
#' @param zarr A data frame containing details of the Zarr resource, including metadata.
#'
#' @return Returns a terra SpatRaster object or data frame containing the processed data.
#' @examples
#' \dontrun{
#' # Example usage (assuming `var` and `zarr` are properly formatted):
#' # result <- zarr_to_terra(var, zarr)
#' }
#' @family zarr
#' @export

zarr_to_terra = function(var, zarr) {
  
  if(zarr$startDate == zarr$endDate){
    dates = as.POSIXct(zarr$startDate, tz = "UTC")
  } else {
    dates <- seq.POSIXt(as.POSIXct(zarr$startDate, tz = "UTC"),
                        as.POSIXct(zarr$endDate, tz = "UTC"),
                        by =  zarr$interval)
    
  }
  
  
  name <-  gsub("_NA", "",paste(zarr$variable, 
                                dates,
                                zarr$model, 
                                zarr$ensemble, 
                                zarr$scenario,
                                sep = "_"))
  
  vars = zarr$variable
  
  if(length(vars) == 0){ vars = zarr$varname }
  
  names_ts = sub("_$", "", 
                 gsub("__", "", gsub("_NA", "", 
                                     paste(
                                       vars,
                                       zarr$model, 
                                       zarr$ensemble, 
                                       zarr$scenario,
                                       sep = "_")))
  )
  
  if (zarr$X1 == zarr$Xn & zarr$Y1 == zarr$Yn) {
    df = data.frame(date = dates, var)
    names(df) = c("date", names_ts)
    return(df)
  }
  
  resx <- zarr$resX#(dap$Xn - dap$X1) / (dap$ncols - 1)
  resy <- zarr$resY#(dap$Yn - dap$Y1) / (dap$nrows - 1)
  
  xmin <- zarr$X1 - 0.5 * resx
  xmax <- zarr$Xn + 0.5 * resx
  ymin <- zarr$Y1 - 0.5 * resy
  ymax <- zarr$Yn + 0.5 * resy
  
  
  if (length(dim(var)) == 2) {
    dim(var) <- c(dim(var), 1)
  }
  
  r = rast(nrows = zarr$nrows,
           ncols = zarr$ncols,
           crs = zarr$crs,
           nlyrs = zarr$Tdim,
           extent = c(
             xmin = min(xmin, xmax),
             xmax = max(xmin, xmax),
             ymin = min(ymin, ymax),
             ymax = max(ymin, ymax)
           ))
  

  terra::values(r) = var
  
  if (zarr$toptobottom) { r <- flip(r) }
  
  units(r) <- zarr$units
  time(r) <- dates
  names(r) <- name
  
  r
}




#' Find DAP Metadata
#' @param raw data.frame
#' @return data.frame
#' @family dap
#' @export

dap_meta = function(raw){
  raw <- variable_meta(raw)
  raw <- time_meta(raw)
  raw <- grid_meta(raw)
  raw
}

#' Find DAP variable metadata
#' @param raw data.frame
#' @param verbose emit messages
#' @return data.frame
#' @family dap
#' @export

variable_meta = function (raw, verbose = TRUE){
  
  if (!"variable" %in% names(raw)) {
    warning("raw must include variable column")
    if (!"variable" %in% names(raw)) {
      warning("trying varname. Chance of failure...")
      raw$variable <- raw$varname
    }
  }
  
  if (all(c("units", "description") %in% names(raw))) {
    if (verbose) {
      message("Variable metadata already exists")
    }
    return(raw)
  } else {
    
    res <- by(raw, list(raw$variable), function(x) {
      c(
        URL = x$URL[1],
        variable = x$variable[1],
        id = x$id[1]
      )
    })
    
    tmp <- data.frame(do.call(rbind, res))
    
    ll <- list()
    
    for (i in 1:nrow(tmp)) {
      ll[[i]] <- tryCatch({
        t <- dap_xyzv(obj = paste0(tmp$URL[i], "#fillmismatch"),
                      varmeta = TRUE)
        t$variable <- tmp$variable[i]
        t
      }, error = function(e) {
        NULL
      })
      if (verbose) {
        message("[",
                tmp$id[i],
                ":",
                tmp$variable[i],
                "] (",
                i,
                "/",
                nrow(tmp),
                ")")
      }
    }
    
    x = do.call(rbind, ll)
    x = x[!names(x) %in% names(raw)]
    return(merge(raw, do.call(rbind, ll), by = "variable"))
  }
}

#' Find DAP time metadata
#' @param raw data.frame
#' @return data.frame
#' @family dap
#' @export

time_meta = function (raw){
  if (all(c("duration", "interval", "nT") %in% names(raw))) {
    message("Time metadata already exists")
    return(raw)
  }
  else {
    flag <- !"scenario" %in% names(raw)
    if (flag) {
      raw$scenario <- "total"
      tmp <- raw[1,]
    }
    else {
      res <- by(raw, list(raw$scenario, raw$id), function(x) {
        c(
          URL = x$URL[1],
          scenario = x$scenario[1],
          id = x$id[1],
          T_name = x$T_name[1]
        )
      })
      tmp <- data.frame(do.call(rbind, res))
    }
    ll <- list()
    for (i in 1:nrow(tmp)) {
      nc <- open.nc(paste0(tmp$URL[i], "#fillmismatch"))
      ll[[i]] <-
        data.frame(
          .resource_time(nc, T_name = tmp$T_name[i]),
          scenario = tmp$scenario[i],
          id = tmp$id[i]
        )
      message("[",
              tmp$id[i],
              ":",
              tmp$scenario[i],
              "] (",
              i,
              "/",
              nrow(tmp),
              ")")
      RNetCDF::close.nc(nc)
    }
    if (flag) {
      raw$duration <- ll[[1]]$duration
      raw$interval <- ll[[1]]$interval
      raw$nT <- ll[[1]]$nT
    }
    else {
      raw <- merge(raw, do.call(rbind, ll), 
                   by = c("scenario", "id"))
    }
    return(raw)
  }
}

#' Find DAP grid metadata
#' @param raw data.frame
#' @return data.frame
#' @family dap
#' @export

grid_meta = function (raw){
  if (all(
    c(
      "T_name",
      "X_name",
      "Y_name",
      "nrows",
      "ncols",
      "X1",
      "Xn",
      "Y1",
      "Yn",
      "crs"
    ) %in% names(raw)
  )) {
    message("Grid metadata already exists")
    return(raw)
  }
  else {
    url <- paste0(raw$URL[1], "#fillmismatch")
    nc <- open.nc(url)
    g <- .resource_grid(nc)
    close.nc(nc)
    return(cbind(raw, g))
  }
}
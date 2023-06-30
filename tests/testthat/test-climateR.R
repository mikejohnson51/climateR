library(AOI)
library(terra)


test_that("climater_filter", {
  expect_equal(nrow(climater_filter(asset = '2019 Land Cover L48')), 1)
  expect_error(climater_filter(id = "BLAH"))
  expect_error(climater_filter(id = "MODIS", asset = "BLAH"))
  expect_equal(nrow(
    climater_filter(
      id = "maca_day",
      varname = "pr",
      model = 1,
      startDate = "2000-01-01"
    )
  ), 1)
  expect_error(climater_filter(
    id = "maca_day",
    varname = "pr",
    model = 1e9,
    startDate = "2000-01-01"
  ))
  expect_error(climater_filter(AOI = aoi_get(country = "Egypt"), asset = '2019 Land Cover L48'))
  expect_equal(nrow(climater_filter(id = "gridmet", varname = "pr")), 1)
})


test_that("TerraClim", {
  out = getTerraClim(
    AOI = aoi_get("Fort Collins"),
    varname = "tmin",
    startDate = "2020-01-01"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "tmin")
  expect_true(names(out[[1]]) == "tmin_2020-01-01_total")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(ncol(out[[1]]) == 5)
  expect_equal(nrow(getTerraClim(
    AOI = aoi_get("Fort Collins"),
    varname = "tmin",
    dryrun = TRUE
  )), 1)
  
  ex = extract_sites(out, geocode("Fort Collins", pt = TRUE), "request")
  
  expect_true(ex[[1]]$FortCollins == -7.2)
  expect_true(ex[[1]]$date == as.POSIXct("2020-01-01", tz = "UTC"))
})

test_that("TerraClimNormals", {
  out = getTerraClimNormals(
    AOI = aoi_get("Fort Collins"),
    varname = "tmin",
    month = 4,
    dryrun = TRUE
  )
  expect_equal(nrow(out), 1)
  
  out = getTerraClimNormals(AOI = aoi_get("Fort Collins"),
                            varname = "tmin",
                            month = 4)
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "tmin")
  expect_true(names(out[[1]]) == "tmin_1961-04-01_19812010")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(ncol(out[[1]]) == 5)
})

test_that("end and start dates", {
  # Monthly
  start = getTerraClim(
    AOI        = aoi_get("Fort Collins"),
    varname    = "aet",
    startDate  = strsplit(climater_filter(id = 'terraclim')$duration[1], "/")[[1]][1]
  )
  
  expect_equal(length(start), 1)
  
  end = getTerraClim(
    AOI      = aoi_get("Fort Collins"),
    varname    = "aet",
    startDate  =   strsplit(climater_filter(id = 'terraclim')$duration[1], "/")[[1]][2]
  )
  
  expect_equal(length(end), 1)
  
  # Daily
  start = getGridMET(
    AOI        = aoi_get("Fort Collins"),
    varname    = "pr",
    startDate  =  strsplit(climater_filter(id = 'gridmet')$duration[1], "/")[[1]][1]
  )
  
  expect_equal(length(start), 1)
  
  end = getGridMET(
    AOI         = aoi_get("Fort Collins"),
    varname    = "pr",
    startDate  =   as.character(Sys.Date() - 2)
  )
  
  expect_equal(length(end), 1)
})

test_that("Daymet", {
  out = getDaymet(
    AOI = aoi_get("Fort Collins"),
    varname = "tmin",
    startDate = "2020-01-01"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "tmin")
  expect_true(names(out[[1]]) == "tmin_2019-12-31 12:00:00_na_total")
  expect_true(nrow(out[[1]]) == 20)
  expect_true(ncol(out[[1]]) == 16)
})

test_that("gridmet", {
  out = getGridMET(
    AOI = aoi_get("Fort Collins"),
    varname = "pr",
    startDate = "2020-01-01"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "precipitation_amount")
  expect_true(names(out[[1]]) == "pr_2020-01-01")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(ncol(out[[1]]) == 5)
  
  out = getGridMET(
    AOI = aoi_get("Fort Collins"),
    varname = "pdsi",
    startDate = "2020-01-01"
  )
  expect_equal(length(out), 2)
})

test_that("loca", {
  out = getLOCA(
    AOI = aoi_get("Fort Collins"),
    varname = "pr",
    startDate = "2020-01-01",
    dryrun = TRUE
  )
  
  expect_equal(nrow(out), 1)
  
  out = getLOCA(
    AOI = aoi_get("Fort Collins"),
    varname = "pr",
    startDate = "2020-01-01"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "pr_CCSM4_r6i1p1_rcp45")
  expect_true(names(out[[1]]) == "pr_2019-12-31 12:00:00_CCSM4_r6i1p1_rcp45")
  expect_true(nrow(out[[1]]) == 4)
  expect_true(ncol(out[[1]]) == 4)
})


test_that("MACA", {
  expect_error(
    getMACA(
      AOI = aoi_get("Fort Collins"),
      varname = "pr",
      model = "CanESM2",
      timeRes = "daily",
      startDate = "2080-01-01"
    )
  )
  # Future
  out = getMACA(
    AOI = aoi_get("Fort Collins"),
    varname = "pr",
    model = "CanESM2",
    startDate = "2080-01-01"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "precipitation")
  expect_true(names(out[[1]])[1] == "pr_2080-01-01_CanESM2_r1i1p1_rcp45")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(ncol(out[[1]]) == 5)
  
  out = getMACA(
    AOI = aoi_get("Fort Collins"),
    varname = "pr",
    startDate = "1970-01-01"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "precipitation")
  expect_true(names(out[[1]])[1] == "pr_1970-01-01_CCSM4_r6i1p1_historical")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(ncol(out[[1]]) == 5)
  
  expect_error(getMACA(
    AOI = aoi_get("Fort Collins"),
    varname = "pr",
    model = "RRR",
    startDate = "2080-01-01"
  ))
  expect_error(getMACA(
    AOI = aoi_get("Fort Collins"),
    varname = "RRR",
    model = "CanESM2",
    startDate = "2080-01-01"
  ))
  expect_error(getMACA(
    AOI = aoi_get("Fort Collins"),
    varname = "pr",
    model = "CanESM2",
    startDate = "3080-01-01"
  ))
  expect_error(getMACA(
    AOI = aoi_get("Fort Collins"),
    varname = "pr",
    model = "CanESM2",
    startDate = "1080-01-01"
  ))
  
})


test_that("NLDAS", {
  # skip on CI - no netrc/dodsrc
  skip_on_ci()
  
  out = getNLDAS(
    AOI = aoi_get("Fort Collins"),
    varname = "ugrd10m",
    startDate = "2020-01-01"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out)[1] == "ugrd10m")
  expect_true(nlyr(out[[1]]) == 24)
  expect_true(nlyr(out[[2]]) == 1)
  
})


test_that("GLDAS", {
  # skip on CI - no netrc/dodsrc
  skip_on_ci()
  
  out = getGLDAS(
    AOI = aoi_get("Fort Collins"),
    varname = "snowt_tavg",
    startDate = "2020-01-01"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out)[1] == "snowt_tavg")
  expect_true(nlyr(out[[1]]) == 1)
})


test_that("MODIS", {
  # skip on CI - no netrc/dodsrc
  skip_on_ci()
  
  dead_url  = tryCatch({
    httr::GET('https://opendap.cr.usgs.gov')
    FALSE
  }, error = function(e) {
    TRUE
  })
  
  out = getMODIS(
    AOI       = aoi_get(state = "FL"),
    asset     = 'MOD16A2.006',
    varname   = "PET_500m",
    startDate = "2020-10-29",
    dryrun = TRUE
  )
  
  expect_true(nrow(out) == 3)
  
  skip_if(dead_url, "MODIS server is down! Skipping tests")
  
  out = getMODIS(
    AOI       = aoi_get("Fort Collins"),
    asset     = 'MOD16A2.006',
    varname   = "PET_500m",
    startDate = "2020-10-29"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out)[1] == "PET_500m")
  expect_true(nlyr(out[[1]]) == 1)
  
  
  state_wide = getMODIS(
    AOI = aoi_get(state = "FL"),
    asset = 'MOD16A2.006',
    varname = "PET_500m",
    startDate = "2020-10-29"
  )
  
  expect_true(class(state_wide) == "list")
  expect_true(class(state_wide[[1]]) == "SpatRaster")
  expect_true(names(state_wide)[1] == "PET_500m")
  expect_true(nlyr(state_wide[[1]]) == 1)
  
})

test_that("PRISM", {
  
  daily = getPRISM(
    AOI = aoi_get("Fort Collins"),
    varname = "tmin",
    startDate = "2015-01-10",
    endDate = NULL,
    dryrun = TRUE
  )
  
  expect_equal(nrow(daily), 1)
  
  prism <- getPRISM(
    AOI = geocode("Fort Collins", pt = TRUE),
    varname = c('tmax', 'tmin'),
    startDate = "2021-01-01",
    endDate = "2021-01-10"
  )
  
  expect_true(nrow(prism) == 10)
  expect_true(ncol(prism) == 3)
  expect_true(all(prism$tmax > prism$tmin))
  
  daily = getPRISM(
    AOI = aoi_get("Fort Collins"),
    varname = "tmin",
    startDate = "2015-01-10",
    endDate = "2015-01-15"
  )
  
  expect_true(class(daily) == "list")
  expect_true(class(daily[[1]]) == "SpatRaster")
  expect_true(names(daily)[1] == "tmin")
  expect_true(nlyr(daily[[1]]) == 6)
  
  monthly = getPRISM(
    AOI = aoi_get("Fort Collins"),
    varname = "tmn",
    timeRes = "monthly",
    startDate = "2015-01-10",
    endDate = "2015-06-15"
  )
  
  expect_true(class(monthly) == "list")
  expect_true(class(monthly[[1]]) == "SpatRaster")
  expect_true(names(monthly)[1] == "tmn")
  expect_true(nlyr(monthly[[1]]) == 6)
  
})

test_that("local & extract pts", {
  f = system.file("nc/bcsd_obs_1999.nc", package = "climateR")
  
  x =  dap(URL = f, verbose = FALSE)
  expect_true(class(x) == "list")
  expect_true(class(x[[1]]) == "SpatRaster")
  expect_true(names(x)[1] == "pr")
  expect_true(nlyr(x[[1]]) == 12)
  
  s <- spatSample(x$pr[[1]], 10, as.points = TRUE, na.rm = T)
  s$id = 1:nrow(s)
  dat = extract_sites(r = x, pts = s, id = "id")
  expect_true(ncol(dat[[1]]) == 11)
  expect_true(nrow(dat[[1]]) == nlyr(x[[1]]))
  expect_true(length(dat) == length(x))
  
})

test_that("Remote VRT", {
  cat = climater_filter(asset = '2019 Land Cover L48')
  nlcd = dap(catalog = cat, AOI = aoi_get("Fort Collins"))
  expect_true(class(nlcd[[1]]) == "SpatRaster")
  expect_true(nlyr(nlcd[[1]]) == 1)
  expect_true(res(nlcd[[1]])[1] == 30)
  
})


terra::rast('/Users/mjohnson/github/opendap.catalog/data-raw/ned/USGS_seamless_13.vrt')

terra::rast('/vsicurl/https://prd-tnm.s3.amazonaws.com/StagedProducts/Elevation/13/TIFF/USGS_Seamless_DEM_13.vrt')

terra::rast('/vsis3/nextgen-hydrofabric/DEM-products/USGS_seamless_13.vrt')

test_that("Livneh", {
  xx = getLivneh(
    AOI = aoi_get("Fort Collins"),
    varname = "wind",
    startDate = "2011-11-29",
    endDate = "2011-12-03"
  )
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "wind")
  expect_true(nlyr(xx[[1]]) == 5)
  
  xx = getLivneh(
    AOI = aoi_get("Fort Collins"),
    timeRes = "monthly",
    varname = "wind",
    startDate = "2011-11-29",
    endDate = "2011-12-03"
  )
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "wind")
  expect_true(nlyr(xx[[1]]) == 2)
  
  xx = getLivneh_fluxes(
    AOI = aoi_get("Fort Collins"),
    varname = "SWE",
    startDate = "2011-11-29",
    endDate = "2011-12-03"
  )
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "SWE")
  expect_true(nlyr(xx[[1]]) == 5)
  
})


test_that("CHIRPS", {
  
  expect_error(getCHIRPS(
    AOI = aoi_get("Fort Collins"),
    startDate = "2011-11-29",
    endDate = "2011-12-03",
    timeRes = "BLAH"
  ))
  
  xx = getCHIRPS(
    AOI = aoi_get("Fort Collins"),
    startDate = "2011-11-29",
    endDate = "2011-12-03"
  )
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "precip")
  expect_true(nlyr(xx[[1]]) == 5)
  
  xx = getCHIRPS(
    AOI = aoi_get("Fort Collins"),
    timeRes = "monthly",
    startDate = "2011-11-29",
    endDate = "2011-12-03"
  )
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "precip")
  expect_true(nlyr(xx[[1]]) == 1)
  
  expect_error(getCHIRPS(
    AOI = aoi_get("Fort Collins"),
    period = "BLAH",
    startDate = "2011-11-29",
    endDate = "2011-12-03"
  ))
})

test_that("piping AOI", {
  xx = aoi_get("Fort Collins") %>%
    getCHIRPS(startDate = "2011-11-29", endDate = "2011-12-03")
  
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "precip")
  expect_true(nlyr(xx[[1]]) == 5)
  
})

test_that("dap_xyzv", {
  f = system.file("nc/bcsd_obs_1999.nc", package = "climateR")
  o = dap_xyzv(f)
  expect_equal(nrow(o), 2)
  expect_equal(o$X_name[1], "longitude")
  expect_equal(o$Y_name[1], "latitude")
  expect_equal(o$T_name[1], "time")
  expect_equal(o$dim_order[1], "TYX")
  
  expect_error(.resource_grid(f))
  
  expect_true(.resource_grid(RNetCDF::open.nc(f))$resX == .125)
  
  expect_error(dap_xyzv(f, varname = "BLAH"))
  
  URL = "https://gpm1.gesdisc.eosdis.nasa.gov/opendap/hyrax/GPM_L3/GPM_3IMERGHH.06/2021/001/3B-HHR.MS.MRG.3IMERG.20210101-S000000-E002959.0000.V06B.HDF5"
  dap = dap_xyzv(URL)
  expect_true(nrow(dap) == 10)
  
  .resource_time(nc = RNetCDF::open.nc(URL))
  
})

test_that("utils", {
  f = system.file("nc/bcsd_obs_1999.nc", package = "climateR")
  
  o = dap_xyzv(f)
  expect_equal(nrow(o), 2)
  expect_equal(o$X_name[1], "longitude")
  expect_equal(o$Y_name[1], "latitude")
  expect_equal(o$T_name[1], "time")
  expect_equal(o$dim_order[1], "TYX")
  
  expect_true(.resource_grid(RNetCDF::open.nc(f))$resX == .125)
  expect_error(dap_xyzv(f, varname = "BLAH"))
  
  expect_error(.resource_grid(f))
  
  cat = dap_crop(f)
  
  expect_equal(nrow(dap_to_local(cat[1, ], get = FALSE)), 1)
  expect_equal(length(dap_to_local(cat[1, ], get = TRUE)),
               (cat$nT[1] * cat$ncols[1] * cat$nrows[1]))
  
})

test_that("BCCA", {
  xx = aoi_get("Fort Collins") %>%
    getBCCA(startDate = "2011-11-29", endDate = "2011-12-03")
  
  expect_true(length(xx) == 6)
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(nlyr(xx[[1]]) == 5)
  
})


test_that("pts", {
  f = system.file("nc/bcsd_obs_1999.nc", package = "climateR")
  pt = geocode("Durham, NC", pt = TRUE)
  
  ts = dap(URL = f,
           AOI = pt,
           verbose = FALSE)
  expect_equal(ncol(ts), 3)
  expect_equal(names(ts), c("date", "pr", "tas"))
  expect_equal(length(ts[[1]]), 12)
  
  full = dap(f, verbose = FALSE)
  expect_equal(length(full), 2)
  expect_equal(names(full), c("pr", "tas"))
  expect_equal(nlyr(full[[1]]), 12)
  
  ext = extract_sites(r = full, pt, id = "request")
  
  expect_true(all(round(ext$tas$`Durham,NC`, 5) == round(ts$tas, 5)))
  expect_equal(length(ext), 2)
  expect_equal(names(ext), c("pr", "tas"))
  expect_equal(nrow(ext[[1]]), 12)
  
  pts = geocode(c("Durham, NC", "Raleigh, NC"), pt = TRUE)
  ext2 = extract_sites(r = full, pts, id = "request")
  expect_equal(nrow(ext2[[1]]), 12)
  expect_equal(ncol(ext2[[1]]), 3)
  
})

test_that("VRT", {
  tmp = filter(params,  id == "HBV")[1, ]
  
  hbv = vrt_crop_get(catalog = tmp, AOI = aoi_get("Fort Collins"))
  
  hbv1 = vrt_crop_get(catalog = tmp,
                      AOI = aoi_get("Fort Collins"),
                      start = 2)
  
  hbv1_2 = vrt_crop_get(
    catalog = tmp,
    AOI = aoi_get("Fort Collins"),
    varname = "FC"
  )
  
  expect_true(all.equal(hbv1, hbv1_2))
  expect_true(names(hbv1) == names(hbv))
  
  hbv_2 = vrt_crop_get(
    catalog = tmp,
    AOI = aoi_get("Fort Collins"),
    start = 2,
    end = 3
  )
  
  expect_true(all.equal(hbv_2[[1]], hbv[[1]][[2:3]]))
  expect_true(all(names(hbv_2[[1]]) == names(hbv[[1]])[2:3]))
  
})

test_that("FTP", {
  
  dr = getLOCA_hydro(
    AOI = aoi_get(state = "FL"),
    varname = "baseflow",
    startDate = "1990-12-31",
    endDate = "1991-01-01",
    dryrun = TRUE
  )
  
  expect_true(nrow(dr) == 2)
  expect_true(ncol(dr) == 6)
  
  oo = getLOCA_hydro(
    AOI = aoi_get(state = "FL"),
    varname = "baseflow",
    startDate = "1990-12-31",
    endDate = "1991-01-01"
  )
  
  expect_true(length(oo) == 1)
  expect_true(nlyr(oo[[1]]) == 2)
  
  oo = getLOCA_hydro(
    AOI = AOI::aoi_get(state = "FL"),
    varname = c("baseflow", "ET"),
    startDate = "1990-12-31",
    endDate = "1991-01-01"
  )
  
  expect_true(length(oo) == 2)
  expect_true(nlyr(oo[[1]]) == 2)
  
})

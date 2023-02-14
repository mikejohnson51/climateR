library(AOI)
library(terra)

AOI = AOI::aoi_get("Fort Collins")

test_that("climater_filter", {
 expect_equal(nrow(climater_filter(asset = '2019 Land Cover L48')), 1)
 expect_error(climater_filter(id = "BLAH"))
 expect_error(climater_filter(id = "MODIS", asset = "BLAH"))
 expect_equal(nrow(climater_filter(id = "maca_day", varname = "pr", model = 1, startDate = "2000-01-01")), 1)
 expect_error(climater_filter(id = "maca_day", varname = "pr", model = 1e9, startDate = "2000-01-01"))
 expect_error(climater_filter(AOI = AOI::aoi_get(country = "Egypt"), asset = '2019 Land Cover L48'))
 expect_equal(nrow(climater_filter(id = "gridmet", varname = "pr")), 1)
})


test_that("TerraClim", {
  out = getTerraClim(AOI = AOI::aoi_get("Fort Collins"), varname = "tmin", startDate = "2020-01-01")
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "tmin_total")
  expect_true(names(out[[1]]) == "2020-01-01")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(terra::ncol(out[[1]]) == 5)
  expect_equal(nrow(getTerraClim(AOI = AOI::aoi_get("Fort Collins"), varname = "tmin", dryrun = TRUE)), 1)
})

test_that("TerraClimNormals", {

  out = getTerraClimNormals(AOI = AOI::aoi_get("Fort Collins"), varname = "tmin", month = 4, dryrun = TRUE)
  expect_equal(nrow(out), 1)

  out = getTerraClimNormals(AOI = AOI::aoi_get("Fort Collins"), varname = "tmin", month = 4)
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "tmin_19812010")
  expect_true(names(out[[1]]) == "1961-04-01")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(ncol(out[[1]]) == 5)
})

test_that("Daymet", {
  out = getDaymet(AOI = AOI::aoi_get("Fort Collins"), varname = "tmin", startDate = "2020-01-01")
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "tmin_total")
  expect_true(names(out[[1]]) == "2019-12-31 12:00:00")
  expect_true(nrow(out[[1]]) == 20)
  expect_true(ncol(out[[1]]) == 16)
})

test_that("gridmet", {
  out = getGridMET(AOI = AOI::aoi_get("Fort Collins"), varname = "pr", startDate = "2020-01-01")
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "precipitation_amount")
  expect_true(names(out[[1]]) == "2020-01-01")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(ncol(out[[1]]) == 5)
  
  out = getGridMET(AOI = AOI::aoi_get("Fort Collins"), varname = "pdsi", startDate = "2020-01-01")
  expect_equal(length(out), 2)
})

test_that("loca", {
  out = getLOCA(AOI = AOI::aoi_get("Fort Collins"), varname = "pr", startDate = "2020-01-01", dryrun = TRUE )
  expect_equal(nrow(out), 1)
  out = getLOCA(AOI = AOI::aoi_get("Fort Collins"), varname = "pr", startDate = "2020-01-01" )
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "pr_CCSM4_r6i1p1_rcp45_rcp45")
  expect_true(names(out[[1]]) == "2019-12-31 12:00:00")
  expect_true(nrow(out[[1]]) == 4)
  expect_true(ncol(out[[1]]) == 4)
})


test_that("MACA", {
  
  expect_error(getMACA(AOI = AOI::aoi_get("Fort Collins"), varname = "pr", model = "CanESM2", timeRes = "daily", startDate = "2080-01-01"))
  # Future
  out = getMACA(AOI = AOI::aoi_get("Fort Collins"), varname = "pr", model = "CanESM2", startDate = "2080-01-01")

  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "precipitation")
  expect_true(names(out[[1]])[1] == "2080-01-01")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(ncol(out[[1]]) == 5)
  
  out = getMACA(AOI = AOI::aoi_get("Fort Collins"), varname = "pr", startDate = "1970-01-01")

  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out) == "precipitation")
  expect_true(names(out[[1]])[1] == "1970-01-01")
  expect_true(nrow(out[[1]]) == 5)
  expect_true(ncol(out[[1]]) == 5)
  
  expect_error(climateR::getMACA(AOI = AOI::aoi_get("Fort Collins"), varname = "pr", model = "RRR", startDate = "2080-01-01"))
  expect_error(climateR::getMACA(AOI = AOI::aoi_get("Fort Collins"), varname = "RRR", model = "CanESM2", startDate = "2080-01-01"))
  expect_error(climateR::getMACA(AOI = AOI::aoi_get("Fort Collins"), varname = "pr", model = "CanESM2", startDate = "3080-01-01"))
  expect_error(climateR::getMACA(AOI = AOI::aoi_get("Fort Collins"), varname = "pr", model = "CanESM2", startDate = "1080-01-01"))
  
})


test_that("NLDAS", {
  
  out = getNLDAS(AOI = AOI::aoi_get("Fort Collins"), varname = "ugrd10m", startDate = "2020-01-01")
 
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out)[1] == "ugrd10m")
  expect_true(nlyr(out[[1]]) == 24)
  expect_true(nlyr(out[[2]]) == 1)

})


test_that("GLDAS", {
  
  out = getGLDAS(AOI = AOI::aoi_get("Fort Collins"), varname = "snowt_tavg", startDate = "2020-01-01")
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out)[1] == "snowt_tavg")
  expect_true(nlyr(out[[1]]) == 1)

  
})


test_that("MODIS", {
  
  out = getMODIS(AOI = AOI::aoi_get("Fort Collins"), 
                 asset = 'MOD16A2.006', 
                 varname = "PET_500m", 
                 startDate = "2020-10-29")
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out)[1] == "PET_500m")
  expect_true(nlyr(out[[1]]) == 1)
  
  
  state_wide = getMODIS(AOI = AOI::aoi_get(state = "FL"), 
                 asset = 'MOD16A2.006', 
                 varname = "PET_500m", 
                 startDate = "2020-10-29")
  
  expect_true(class(state_wide) == "list")
  expect_true(class(state_wide[[1]]) == "SpatRaster")
  expect_true(names(state_wide)[1] == "PET_500m")
  expect_true(nlyr(state_wide[[1]]) == 1)

})

test_that("PRISM", {
  
  daily = getPRISM(AOI = AOI::aoi_get("Fort Collins"),
           varname = "tmin",
           startDate = "2015-01-10", endDate = NULL, dryrun = TRUE)
  
  expect_equal(nrow(daily), 1)
  
  daily = getPRISM(AOI = AOI::aoi_get("Fort Collins"),
                   varname = "tmin",
                   startDate = "2015-01-10", endDate = "2015-01-15")
  
  expect_true(class(daily) == "list")
  expect_true(class(daily[[1]]) == "SpatRaster")
  expect_true(names(daily)[1] == "tmin")
  expect_true(nlyr(daily[[1]]) == 6)
  
  monthly = getPRISM(AOI = AOI::aoi_get("Fort Collins"),
           varname = "tmn",
           timeRes = "monthly",
           startDate = "2015-01-10", endDate = "2015-06-15")
  
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
 
 s <- spatSample(x$pr[[1]], 10, as.points=TRUE, na.rm = T)
 s$id = 1:nrow(s)
 dat = extract_sites(r = x, pts = s, id = "id")
 expect_equal(sum(filter(dat$pr, date == '1999-01-31 00:00:00')[,-1] == s$`1999-01-31 00:00:00`), nrow(s))
  
})


test_that("local & extract pts", {
  
  cat = climater_filter(asset = '2019 Land Cover L48') 
  nlcd = dap(catalog = cat, AOI = AOI)
  expect_true(class(nlcd) == "SpatRaster")
  expect_true(nlyr(nlcd) == 1)
  expect_true(res(nlcd)[1] == 30)

})


test_that("Livneh", {
  
  xx = getLivneh(AOI = AOI::aoi_get("Fort Collins"), varname = "wind", startDate = "2011-11-29", endDate = "2011-12-03")
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "wind")
  expect_true(nlyr(xx[[1]]) == 5)

  xx = getLivneh(AOI = AOI::aoi_get("Fort Collins"), timeRes = "monthly", varname = "wind", startDate = "2011-11-29", endDate = "2011-12-03")
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "wind")
  expect_true(nlyr(xx[[1]]) == 2)
  
  xx = getLivneh_fluxes(AOI = AOI::aoi_get("Fort Collins"), varname = "SWE", startDate = "2011-11-29", endDate = "2011-12-03")
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "SWE")
  expect_true(nlyr(xx[[1]]) == 5)
  
})


test_that("CHIRPS", {
  
  xx = getCHIRPS(AOI = AOI::aoi_get("Fort Collins"), startDate = "2011-11-29", endDate = "2011-12-03")
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "precip")
  expect_true(nlyr(xx[[1]]) == 5)
  
  xx = getCHIRPS(AOI = AOI::aoi_get("Fort Collins"), period = "monthly", startDate = "2011-11-29", endDate = "2011-12-03")
  expect_true(class(xx) == "list")
  expect_true(class(xx[[1]]) == "SpatRaster")
  expect_true(names(xx)[1] == "precip")
  expect_true(nlyr(xx[[1]]) == 1)
  
  expect_error(getCHIRPS(AOI = AOI::aoi_get("Fort Collins"), period = "BLAH", startDate = "2011-11-29", endDate = "2011-12-03"))
 
 
  
})


suppressWarnings({ library(AOI) })
suppressWarnings({ library(terra) })
library(climateR)

cities =  geocode(c("Fort Collins, CO", 
                    "Durham, NC", 
                    "Raleigh, NC"), 
                  pt = TRUE)

bb     =  geocode("Fort Collins, CO", bb = TRUE)


test_that("NLDAS", {
  # skip on CI - no netrc/dodsrc
  skip_on_ci()
  
  out = getNLDAS(
    AOI = bb,
    varname = "ugrd10m",
    startDate = "2020-01-01"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out)[1] == "ugrd10m")
  expect_true(nlyr(out[[1]]) == 24)
  
})


test_that("GLDAS", {
  # skip on CI - no netrc/dodsrc
  skip_on_ci()
  
  out = getGLDAS(
    AOI = bb,
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
    asset = 'MOD16A3GF.061',
    varname   = "PET_500m",
    startDate = "2020-10-29",
    dryrun = TRUE
  )

  expect_true(nrow(out) == 4)
  
  skip_if(dead_url, "MODIS server is down! Skipping tests")
  
  out = getMODIS(
    AOI       = bb,
    asset = 'MOD16A3GF.061',
    varname   = "PET_500m",
    startDate = "2020-10-29"
  )
  
  expect_true(class(out) == "list")
  expect_true(class(out[[1]]) == "SpatRaster")
  expect_true(names(out)[1] == "PET_500m")
  expect_true(nlyr(out[[1]]) == 1)
  
  
  state_wide = getMODIS(
    AOI = aoi_get(state = "FL"),
    asset = 'MOD16A3GF.061',
    varname = "PET_500m",
    startDate = "2020-10-29"
  )
  
  expect_true(class(state_wide) == "list")
  expect_true(class(state_wide[[1]]) == "SpatRaster")
  expect_true(names(state_wide)[1] == "PET_500m")
  expect_true(nlyr(state_wide[[1]]) == 1)
  
})

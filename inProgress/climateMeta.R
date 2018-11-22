climate = list(

macaV2metdata = list(
  PARAM = data.frame(
    parameter = c('tasmax','tasmin','rhsmax',
                  'rhsmin','pr','rsds',
                  'uas','vas','huss'),

    longname = c(   'Maximum daily temperature near surface',
                    'Minimum daily temperature near surface',
                    'Maximum daily relative humidity near surface',
                    'Minimum daily relative humdity near surface',
                    'Average daily precipitation amount at surface',
                    'Average daily downward shortwave radiation at surface',
                    'Average daily eastward component of wind near surface',
                    'Average daily northward component of wind near surface',
                    'Average daily specific humidity near surface'),

    units = c("K", "K", "%", "%", "mm", "W m-2", "m s-1", "m s-1", "kg kg-1"),

    scale_factor = c(0.1, 0.1, 1, 1, 0.1, 1, 0.1, 0.1, 1.0E-5),
     stringsAsFactors = F
  ),

  models = data.frame(
  name = c("bcc-csm1-1",     "bcc-csm1-1-m",   "BNU-ESM",        "CanESM2",
             "CCSM4",          "CNRM-CM5",       "CSIRO-Mk3-6-0",  "GFDL-ESM2M",
             "GFDL-ESM2G",     "HadGEM2-ES",     "HadGEM2-CC",     "inmcm4",
             "IPSL-CM5A-LR",   "IPSL-CM5A-MR",   "IPSL-CM5B-LR",   "MIROC5",
             "MIROC-ESM",      "MIROC-ESM-CHEM", "MRI-CGCM3",   "NorESM1-M" ),

 country = c("China",          "China",          "China",          "Canada",
             "USA",            "France",        "Australia",      "USA",
             "USA",            "United Kingdom", "United Kingdom", "Russia",
             "France",        "France",         "France",         "Japan",
             "Japan",          "Japan",          "Japan",          "Norway" ),

 agency = c("Beijing Climate Center, China Meteorological Administration",
   "Beijing Climate Center, China Meteorological Administration",
   "College of Global Change and Earth System Science, Beijing Normal University, China",
   "Canadian Centre for Climate Modeling and Analysis",
   "National Center of Atmospheric Research, USA",
   "National Centre of Meteorological Research, France",
   "Commonwealth Scientific and Industrial Research Organization/Queensland Climate Change Centre of Excellence, Australia",
   "NOAA Geophysical Fluid Dynamics Laboratory, USA",
   "NOAA Geophysical Fluid Dynamics Laboratory, USA",
   "Met Office Hadley Center, UK",
   "Met Office Hadley Center, UK",
   "Institute for Numerical Mathematics, Russia",
   "Institut Pierre Simon Laplace, France",
   "Institut Pierre Simon Laplace, France",
   "Institut Pierre Simon Laplace, France",
   "Atmosphere and Ocean Research Institute (The University of Tokyo),\n\t\tNational Institute for Environmental Studies,and Japan Agency for Marine-Earth Science and Technology",
   "Japan Agency for Marine-Earth Science and Technology, Atmosphere and Ocean Research Institute (The University of Tokyo), and National Institute for Environmental Studies",
   "Japan Agency for Marine-Earth Science and Technology, Atmosphere and Ocean Research Institute (The University of Tokyo), and National Institute for Environmental Studies",
   "Meteorological Research Institute, Japan",
   "Norwegian Climate Center, Norway"),

 AtmoRes_lonlat = c("2.8 deg x 2.8 deg",  "1.12 deg x 1.12 deg",
               "2.8 deg x 2.8 deg",   "2.8 deg x 2.8 deg",
               "1.25 deg x 0.94 deg", "1.4 deg x 1.4 deg",
               "1.8 deg x 1.8 deg",   "2.5 deg x 2.0 deg",
               "2.5 deg x 2.0 deg",   "1.88 deg x 1.25 deg",
               "1.88 deg x 1.25 deg", "2.0 deg x 1.5 deg",
               "3.75 deg x 1.8 deg",  "2.5 deg x 1.25 deg",
               "2.75 deg x 1.8 deg",  "1.4 deg x 1.4 deg",
               "2.8 deg x 2.8 deg",   "2.8 deg x 2.8 deg",
               "1.1 deg x 1.1 deg",   "2.5 deg x 1.9 deg"),

ensemble = c( "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1", "r6i1p1",
              "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1",
              "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1",
              "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1", "r1i1p1"),
stringsAsFactors = FALSE),

secenarios = c('rcp45', 'rcp85'),

metadata = data.frame(res = '4km',
                        spatialExtent = "Contiguous USA",
                        dataum = "WGS 84",
                        startDate = "2006-01-01",
                        endData = "2099-12-31",
                        leapDays = 'Yes',
                        stringsAsFactors = F)


),


# UofI Data ---------------------------------------------------------------


UofIMETDATA = list(
  PARAM = data.frame(
param = c('precipitation_amount',
          'max_air_temperature',
          'min_air_temperature',
          'surface_downwelling_shortwave_flux_in_air',
          'specific_humidity',
          'max_relative_humidity',
          'min_relative_humidity',
          'wind_speed'),

description = c('precipitation',
                'maximum temperature',
                'minimum temperature',
                'downward shortwave solar radiation',
                'specific humidity',
                'maximum relative humidity',
                'minimum relative humidity',
                'wind speed'),

unit = c('mm', 'K', 'K', 'W m-2', 'kg/kg', '%', '%', 'm/s'),

scale_factor = c(0.1, 0.1, 0.1, 0.1, 0.001, 0.1, 0.1, 0.1), stringsAsFactors = TRUE
)
)
)



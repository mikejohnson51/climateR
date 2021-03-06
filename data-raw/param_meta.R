## code to prepare `param_meta` dataset goes here
#' @title Parameter Metadata
#' @description  Meta data for model parameters including common names, model calls, description and units
#' @export


param_meta = list(
  # PRISM -------------------------------------------------------------------
  
  prism = data.frame(
    common.name = c('prcp', 'tmax', 'tmin'),
    call = c('ppt', 'tmax', 'tmin'),
    description = c('daily_precipitation', 'daily_maximum_temperture', 'daily_minimum_temperture'),
    units = c('mm', 'degC', 'degC'),
    stringsAsFactors = FALSE
  ),
  
  # GridMET -----------------------------------------------------------------
  
  gridmet = data.frame(
    
    common.name = c('prcp','rhmax',
                    'rhmin','shum',
                    'srad','wind_dir',
                    'tmin', 'tmax',
                    'wind_vel','burn_index',
                    'fmoist_100','fmoist_1000',
                    'energy_release', 'palmer',
                    'pet_alfalfa','pet_grass',
                    'vpd'),
    
    call         = c('pr','rmax',
                     'rmin', 'sph',
                     'srad','th',
                     'tmmn','tmmx',
                     'vs','bi',
                     'fm100','fm1000',
                     'erc', 'pdsi',
                     'etr', 'pet',
                     'vpd'),
    
    description = c('precipitation_amount', 'daily_maximum_relative_humidity',
                    'daily_minimum_relative_humidity', 'daily_mean_specific_humidity',
                    'daily_mean_shortwave_radiation_at_surface', 'daily_mean_wind_direction',
                    'daily_minimum_temperature', 'daily_maximum_temperature',
                    'daily_mean_wind_speed', 'daily_mean_burning_index_g',
                    'dead_fuel_moisture_100hr', 'dead_fuel_moisture_1000hr',
                    'daily_mean_energy_release_component-g', 'daily_mean_palmer_drought_severity_index',
                    'daily_mean_reference_evapotranspiration_alfalfa', 'daily_mean_reference_evapotranspiration_grass',
                    'daily_mean_vapor_pressure_deficit'),
    
    timestep = c('daily','daily',
                 'daily', 'daily',
                 'daily','daily',
                 'daily','daily',
                 'daily','daily',
                 'daily','daily',
                 'daily', 'pentad',
                 'daily', 'daily',
                 'daily'),
    
    units = c("mm", "Percent",
              "Percent", "kg/kg",
              "W/m^2", "Degrees Clockwise from north",
              "degK", "degK",
              "m/s", "Unitless",
              "Percent", "Percent",
              "Unitless", "Unitless",
              "mm", "mm",
              "kPa"),
    
    stringsAsFactors = FALSE),
  
  
  # TopoWX --------------------------------------------------------------------
  
  topowx = data.frame(
    common.name = c('tmax', 'tmin'),
    call = c('tmax', 'tmin'),
    description = c('maximum air temperature', 'minimum air temperature'),
    units = c('degC', 'degC'),
    stringsAsFactors = FALSE
  ),
  
  
  # DAYMET ------------------------------------------------------------------
  
  daymet = data.frame(
    common.name = c('daylength', 'prcp',
                    'srad', 'swe',
                    'tmax', 'tmin',
                    'vp'),
    
    call = c('dayl', 'prcp',
             'srad', 'swe',
             'tmax', 'tmin',
             'vp'),
    
    description = c('daylength', 'daily total precipitation',
                    'daylight average incident shortwave radiation', 'snow water equivalent',
                    'daily maximum temperature', 'daily minimum temperature',
                    'daily average vapor pressure'),
    
    units = c('seconds', 'mm/day',
              'W/m^2', 'kg/m^2',
              'degC', 'degC',
              'Pa'),
    
    stringsAsFactors = FALSE
  ),
  
  
  
  # NCEP --------------------------------------------------------------------
  
  # NCEP --------------------------------------------------------------------
  
  ncep = data.frame(
    
    common.name = c('tavg',
                    'cfnlf', 'cfnsf',
                    'cprat', 'csdlf',
                    'csdsf', 'csusf',
                    'dlwrf', 'dswrf',
                    'gflux', 'hgt',
                    'icec', 'landmask',
                    'lhtfl', 'nbdsf',
                    'nddsf', 'nlwrs',
                    'pevpr', 'prate',
                    'pres', 'runof',
                    'sfcr', 'shtfl',
                    'shum', 'skt',
                    'soilw10', 'soilw200',
                    'tcdc', 'tmax',
                    'tmin', 'btmp10',
                    'btmp200', 'btmp300',
                    'hgt', 'uflx',
                    'ugwd', 'ulwrf',
                    'uswrf', 'uwnd',
                    'vbdsf', 'vddsf',
                    'vflx', 'vgwd',
                    'vwnd','weasd'),
    
    abb = c('air',
            'cfnlf', 'cfnsf',
            'cprat','csdlf',
            'csdsf', 'csusf',
            'dlwrf', 'dswrf',
            'gflux', 'hgt',
            'icec',  'land',
            'lhtfl', 'nbdsf',
            'nddsf', 'nlwrs',
            'pevpr', 'prate',
            'pres',  'runof',
            'sfcr',  'shtfl',
            'shum',  'skt',
            'soilw', 'soilw',
            'tcdc',  'tmax',
            'tmin',  'tmp',
            'tmp',   'tmp',
            'hgt',   'uflx',
            'ugwd',  'ulwrf',
            'uswrf', 'uwnd',
            'vbdsf', 'vddsf',
            'vflx',  'vgwd',
            'vwnd',  'weasd'
    ),
    
    call = c('air.2m',
             'cfnlf.sfc', 'cfnsf.sfs',
             'cprat.sfc', 'csdlf.sfc',
             'csdsf.sfc', 'csusf.sfc',
             'dlwrf.sfc', 'dswrf.sfc',
             'gflux.sfc', 'hgt.sfc',
             'icec.sfc',  'land.sfc',
             'lhtfl.sfc', 'nbdsf.sfc',
             'nddsf.sfc', 'nlwrs.sfc',
             'pevpr.sfc', 'prate.sfc',
             'pres.sfc',  'runof.sfc',
             'sfcr.sfc',  'shtfl.sfc',
             'shum.2m',   'skt.sfc',
             'soilw.0-10cm', 'soilw.10-200cm',
             'tcdc.eatm', 'tmax.2m',
             'tmin.2m','tmp.0-10cm',
             'tmp.10-200cm', 'tmp.300cm',
             'topo.sfc', 'uflx.sfc',
             'ugwd.sfc', 'ulwrf.sfc',
             'uswrf', 'uwnd.10m',
             'vbdsf.sfc', 'vddsf.sfc',
             'vflx.sfc', 'vgwd.sfc',
             'vwnd.10m', 'weasd.sfc'
    ),
    
    description = c('temperature at 2 m',
                    'Cloud Forcing Net Longwave Flux at Surface', 'Cloud Forcing Net Solar Flux at Surface',
                    'Convective Precipitation Rate at surface', 'Clear Sky Downward Longwave Flux at surface',
                    'Clear Sky Downward Solar Flux at surface', 'Clear Sky Upward Solar Flux at surface',
                    'Daily Downward Longwave Radiation Flux at surface',  'Daily Downward Solar Radiation Flux at surface',
                    'Ground Heat Flux at surface', 'Geopotential Height at the Surface',
                    'Daily Ice Concentration at surface', 'Daily Land-sea mask',
                    'Daily Latent Heat Net Flux at surface', 'Daily Near IR Beam Downward Solar Flux at surface',
                    'Daily Near IR Diffuse Downward Solar Flux at surface', 'Daily Net Longwave Radiation Flux at Surface',
                    'Daily Potential Evaporation Rate at surface', 'Daily Precipitation Rate at surface',
                    'Daily Surface Pressure', 'Daily Water Runoff at surface',
                    'Surface Roughness', 'Sensible Heat Net Flux at surface',
                    'Daily Specific Humidity at 2 m', 'Daily SST/Land Skin Temperature',
                    'Daily Volumetric Soil Moisture between 0-10 cm Below Ground Level', 'Daily Volumetric Soil Moisture between 10-200 cm Below Ground Level',
                    'Total cloud cover', 'Daily Maximum Temperature at 2 m',
                    'Daily Minimum Temperature at 2 m', 'Daily Temperature between 0-10 cm below ground level',
                    'Daily Temperature between 10-200 cm below ground level', 'Daily Temperature at 300 cm below ground level',
                    'Geopotential Height at the Surface', 'Daily Momentum Flux, u-component at surface',
                    'Daily Zonal Gravity Wave Stress at surface', 'Daily Upward Longwave Radiation Flux at surface',
                    'Daily Upward Solar Radiation Flux at surface', 'Daily u-wind at 10 m',
                    'Daily Visible Beam Downward Solar Flux at surface', 'Daily Visible Diffuse Downward Solar Flux at surface',
                    'Daily Momentum Flux, v-component at surface', 'Daily Meridional Gravity Wave Stress at surface',
                    'Daily v-wind at 10 m', 'Daily Water Equiv. of Accum. Snow Depth at surface'),
    
    units = c('degK',
              'W/m^2', 'W/m^2',
              'Kg/m^2/s', 'W/m^2',
              'W/m^2', 'W/m^2',
              'W/m^2', 'W/m^2',
              'W/m^2', 'm',
              'NA', 'NA',
              'W/m^2','W/m^2',
              'W/m^2', 'W/m^2',
              'W/m^2', 'Kg/m^2/s',
              'Pa', 'kg/m^2',
              'm', 'W/m^2',
              'kg/kg', 'degK',
              'fraction','fraction',
              'Percent', 'degK',
              'degK','degK',
              'degK', 'degK',
              'm','N/m^2',
              'N/m^2','W/m^2',
              'W/m^2', 'm/s',
              'W/m^2', 'W/m^2',
              'N/m^2','N/m^2',
              'm/s', 'kg/m^2'
    ),
    
    
    stringsAsFactors = FALSE),
  
  
  # MACA --------------------------------------------------------------------
  
  maca = data.frame(
    
    common.name = c('tmax', 'tmin', 'rhmax', 'rhmin', 'prcp', 'srad', 'uwind', 'vwind', 'shum'),
    
    
    call = c('tasmax','tasmin','rhsmax',
             'rhsmin','pr','rsds',
             'uas','vas','huss'),
    
    call2 = c('air_temperature','air_temperature',
              'relative_humidity', 'relative_humidity',
              'precipitation','surface_downwelling_shortwave_flux_in_air',
              'eastward_wind','northward_wind',
              'specific_humidity'),
    
    
    
    description = c('Maximum daily temperature near surface',
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
  
  loca = data.frame(
    
    common.name = c('tmax', 'tmin', 'prcp'),
    call = c('tasmax','tasmin', 'pr'),
    description = c('Maximum daily temperature near surface',
                    'Minimum daily temperature near surface',
                    'Precipitation Rate'),
    units = c("K", "K", "kg m-2 s-1"),
    
    stringsAsFactors = F
  ),
  
  
  bcca = data.frame(
    
    common.name = c('tmax', 'tmin', 'prcp'),
    
    call = c('tasmax','tasmin', 'pr'),
    
    description = c('Maximum daily temperature near surface',
                    'Minimum daily temperature near surface',
                    'Precipitation Rate'),
    
    units = c("K", "K", "mm/day"),
    
    stringsAsFactors = F
    
  ),
  
  sarrd = data.frame(
    
    common.name = c('tmax', 'tmin', 'prcp'),
    
    call = c('tmax','tmin', 'pr'),
    
    description = c('Maximum daily temperature near surface',
                    'Minimum daily temperature near surface',
                    'Precipitation Rate'),
    
    units = c("C", "C", "mm/day"),
    
    stringsAsFactors = F
    
  ),
  
  bcsd = data.frame(
    
    common.name = c('baseflow', 'et', 'petnatveg', 'petshort',
                    'pettall', 'petvegnocr', 'petwater', 'rhum',
                    'rnet', 'smc', 'surface_runoff', 'swe',
                    'total_runoff'),
    
    call = c('baseflow', 'et', 'petnatveg', 'petshort',
             'pettall', 'petvegnocr', 'petwater', 'rhum',
             'rnet', 'smc', 'surface_runoff', 'swe',
             'total_runoff'),
    
    description = c('baseflow', 'et', 'petnatveg', 'petshort',
                    'pettall', 'petvegnocr', 'petwater', 'rhum',
                    'rnet', 'smc', 'surface_runoff', 'swe',
                    'total_runoff'),
    
    units = c('mm/month', 'mm/month', 'mm/month', 'mm/month',
              'mm/month', 'mm/month', 'mm/month', 'percent',
              'W/m2', 'mm', 'mm/month', 'mm',
              'mm/month'),
    
    stringsAsFactors = F
    
  ),
  
  terraclim = data.frame(
    
    common.name = c('aet', 'water_deficit',
                    'palmer', 'pet',
                    'prcp', 'q',
                    'soilm', 'srad',
                    'swe', 'tmax',
                    'tmin', 'vp',
                    'vpd', 'wind'),
    
    call = c('aet', 'def',
             'PDSI', 'pet',
             'ppt', 'q',
             'soil', 'srad',
             'swe', 'tmax',
             'tmin', 'vap',
             'vpd', 'ws'),
    
    description = c('Actual Evapotranspiration', 'Climatic Water Deficit',
                    'Palmer Drought Severity Index', 'Reference Evapotranspiration',
                    'Accumulated Precipitation', 'Runoff',
                    'Soil Moisture at End of Month', 'Downward Shortwave Radiation Flux at the Surface',
                    'Snow Water Equivalent at End of Month', 'Maximum 2-m Temperature',
                    'Minimum 2-m Temperature', '2-m Vapor Pressure',
                    'Mean Vapor Pressure Deficit', 'Wind Speed at 10-m'),
    
    units = c('mm', 'mm',
              '', 'mm',
              'mm', 'mm',
              'mm', 'W/m2',
              'mm', 'C',
              'C', 'kPa',
              'kPa', 'm/s'),
    
    stringsAsFactors = F
    
  ),
  
  
  # CA-BCM --------------------------------------------------------------------
  
  
  cabcm = data.frame(
    
    common.name = c('excess', 'snowpack',
                    'pet', 'prcp', 'recharge',
                    'runoff', "snowfall", "soil",
                    'tmin', 'tmax', 'aet', "cwd"),
    
    call = c('exc', 'pck',
             'pet', 'ppt', 'rch',
             'run', "snw", "str",
             'tmn', 'tmx', 'aet', 'cwd'),
    
    description = c('Excess Water', "Snowpack",
                    'Potential Evapotranspiration', 'Precipitation', 'Recharge',
                    'Runoff', "Snowfall", "Soil Water Storage",
                    'Minimum Temperature', 'Maximum Temperature', 'Actual Evapotranspiration', 'Climatic Water Deficit'),
    
    units = c('mm', 'mm',
              'mm', 'mm', 'mm',
              'mm', 'mm', 'mm',
              'C', 'C', 'mm', 'mm'),
    
    stringsAsFactors = F
    
  )
)
  


usethis::use_data(param_meta, overwrite = TRUE)



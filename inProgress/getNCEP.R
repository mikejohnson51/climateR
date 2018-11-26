

getNCEP = function(AOI, param, param, startDate, endDate)

  if(is.null(endDate)){ endDate = startDate}

g = define.grid(AOI, service = 'ncep', proj = '+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs' )

startDate = '2018-12-29'
endDate  = '2019-01-05'

seq = seq.Date(as.Date(startDate), as.Date(endDate), 1)

x = data.frame(
  year = format(as.Date(seq), "%Y"),
  index.1 = (as.numeric(format(as.Date(seq), "%j"))* 4) - 4,
  index.2 = (as.numeric(format(as.Date(seq), "%j")) * 4) - 3,
  index.3 = (as.numeric(format(as.Date(seq), "%j")) * 4) - 2,
  index.4 = (as.numeric(format(as.Date(seq), "%j")) * 4) - 1,
stringsAsFactors = FALSE)


#date.index = seq.Date(as.Date(startDate), as.Date(endDate), by = 1) - as.Date('1979-01-01')

#dates = format(seq.Date(as.Date(startDate), as.Date(endDate), by = 1), format = "%Y%m%d")

bb = AOI::bbox_st(AOI)

load('/Users/mikejohnson/Documents/GitHub/climateR/data/grids.rda')

ymin = which.min(abs(grids$ncep$lat  - bb$ymin))
ymax = which.min(abs(grids$ncep$lat  - bb$ymax))
xmin = which.min(abs(grids$gridmet$long - bb$xmin))
xmax = which.min(abs(grids$gridmet$long - bb$xmax))

s = list()


paste0('http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis/surface_gauss/'
       air.2m
       '.gauss.'
       1948
       '.nc?'
       air
       [0:1:0] #time
       [0:1:0] #lat
       [0:1:0] #long

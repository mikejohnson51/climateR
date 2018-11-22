define.initial = function(grid, dates){

  if(grid$type == 'point'){
    l = length(dates$date)
    s = data.frame(source = rep("gridmet", l), lat = rep(grid$lat, l), lon = rep(grid$lon, l),  date = dates$date, stringsAsFactors = FALSE)
  } else {
    s = list()
  }

  return(s)
}

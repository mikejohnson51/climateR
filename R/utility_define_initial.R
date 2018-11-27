define.initial = function(grid, dates, param = NULL, dataset = NULL){

  if(grid$type == 'point'){
    l = length(dates$date)
    df = data.frame(source = rep(dataset, l), lat = rep(grid$lat, l), lon = rep(grid$lon, l),  date = dates$date, stringsAsFactors = FALSE)
    s = replicate(NROW(param), df, simplify = FALSE)
    names(s) = param$common.name
  } else {
    s = list()
  }

  return(s)
}


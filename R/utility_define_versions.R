#' @title Define climateR versions
#' @description **INTERNAL** Define if data belongs to separate 
#' (historic and future) archives based on a defiend seperation dat.
#' @param dates a vector of dates
#' @param first.date the first date in a dataset
#' @param sep.date the data in which seperates TDS catolouges
#' @param scenario the climate scenario
#' @param future.call the TDS future catologue
#' @param historic.call the TDS historic catologue
#' @param timeRes the time resolution of dataset (e.g. monthly or daily)
#' @return data.frame

define.versions = function(dates, 
                           first.date = '1950-01-01', sep.date = "2006-01-01", 
                           scenario, future.call, historic.call, timeRes = 'daily'){


  versions = data.frame(min.date = as.Date(character()), max.date = as.Date(character()),
                        time.index = character(), calls = character(), ver = character(),
                        stringsAsFactors = F)

  future   = dates[dates$date >= sep.date,]
  historic = dates[dates$date < sep.date,]

  if(NROW(future) > 0){

   if(timeRes == 'monthly'){ index = future$month.index} else { index = future$date.index }

    for(i in scenario){
      versions[nrow(versions) + 1, ] = list(min.date = min(future$date),
                                            max.date = max(future$date),
                                            time.index = paste0(paste0("[", min(index) , ":1:", max(index), "]")),
                                            calls = future.call,
                                            ver = i)
    }
  }

  if(nrow(historic) > 0){

    if(timeRes == 'monthly'){ index = historic$month.index} else { index = historic$date.index }

    versions[nrow(versions) + 1, ] = list(min.date = min(historic$date),
                                          max.date = max(historic$date),
                                          time.index = paste0(paste0("[", min(index) , ":1:", max(index), "]")),
                                          calls      = historic.call,
                                          ver        = 'historical')

  }

  return(versions)
}

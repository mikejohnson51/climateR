# define.versions = function(dates, first.date = '1950-01-01', sep.date = "2006-01-01", scenario, future.call, historic.call, timeRes = 'daily'){
#
#   dates = as.Date(dates)
#   first.date = as.Date(first.date)
#   sep.date = as.Date(sep.date)
#
#   versions = data.frame(min.date = as.Date(character()), max.date = as.Date(character()),
#                         time.index = character(), calls = character(), ver = character(),
#                         stringsAsFactors = F)
#
#   future   = dates[dates >= sep.date]
#   historic = dates[dates < sep.date]
#
#   if(length(future) > 0){
#
#       index = future - sep.date
#
#         for(i in scenario){
#           versions[NROW(versions) + 1, ] = list(min.date = min(future),
#                       max.date = max(future),
#                       time.index = paste0(paste0("[", min(index) , ":1:", max(index), "]")),
#                       calls = future.call,
#                       ver = i)
#         }
#       }
#
#   if(length(historic) != 0){
#
#     index = historic - first.date
#
#     versions[NROW(versions) + 1, ] = list(min.date = min(historic),
#                 max.date = max(historic),
#                 time.index = paste0(paste0("[", min(index) , ":1:", max(index), "]")),
#                 calls      = historic.call,
#                 ver        = 'historical')
#
#   }
#
#   return(versions)
# }


define.versions = function(dates, first.date = '1950-01-01', sep.date = "2006-01-01", scenario, future.call, historic.call, timeRes = 'daily'){


  versions = data.frame(min.date = as.Date(character()), max.date = as.Date(character()),
                        time.index = character(), calls = character(), ver = character(),
                        stringsAsFactors = F)

  future   = dates[dates$date >= sep.date,]
  historic = dates[dates$date < sep.date,]

  if(NROW(future) > 0){

   if(timeRes == 'monthly'){ index = future$month.index} else { index = future$date.index }

    for(i in scenario){
      versions[NROW(versions) + 1, ] = list(min.date = min(future$date),
                                            max.date = max(future$date),
                                            time.index = paste0(paste0("[", min(index) , ":1:", max(index), "]")),
                                            calls = future.call,
                                            ver = i)
    }
  }

  if(NROW(historic) > 0){

    if(timeRes == 'monthly'){ index = historic$month.index} else { index = historic$date.index }

    versions[NROW(versions) + 1, ] = list(min.date = min(historic$date),
                                          max.date = max(historic$date),
                                          time.index = paste0(paste0("[", min(index) , ":1:", max(index), "]")),
                                          calls      = historic.call,
                                          ver        = 'historical')

  }

  return(versions)
}

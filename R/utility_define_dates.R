#' @title Days in Month
#' @description **INTERNAL** From a date, return the number of days in that month
#' @param date (character) starting date in the form YYYY-MM-DD.
#' @return the number of days in current month
#' @keywords internal

.numberOfDays <- function(date) {
  if(class(date) != 'Date') { date = as.Date(date) }
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format="%d")))
}

#' @title Months between dates
#' @description  **INTERNAL**Find the number of months between dates
#' @param startDate (character) starting date in the form YYYY-MM-DD.
#' @return the number of days in current month
#' @keywords internal

.numberOfMonths = function(date, baseDate) {
  .monnb <- function(d) { lt <- as.POSIXlt(as.Date(d, origin = "1900-01-01"))
  lt$year*12 + lt$mon }

  return(.monnb(date) - .monnb(baseDate))
}


#' @title string to start and end dates
#' @description  **INTERNAL** convert strings to start and end dates
#' @param startDate (character) starting date
#' @param startDate (character) ending date
#' @return the number of days in current month
#' @keywords internal


build.date = function(startDate, endDate){

  if(is.null(endDate)){ endDate = startDate}

  if(nchar(startDate) == 4){ startDate = paste0(startDate, "-01-01") }
  if(nchar(endDate)   == 4){ endDate = paste0(endDate,   "-12-31")  }

  days = .numberOfDays(as.Date(paste0(endDate, "-01")))

  if(nchar(startDate) == 7){ startDate = paste0(startDate, "-01") }
  if(nchar(endDate)   == 7){ endDate = paste0(endDate, "-", days) }

  if(nchar(startDate) == 10){ startDate = startDate }
  if(nchar(endDate)   == 10){ endDate   = endDate }

  return(list(startDate = startDate, endDate = endDate))
}


#' @title Define Date Matrix
#' @description  **INTERNAL** From a user supplied start and end date gnerate the julien, string, and date representations of the data used in climateR
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is the startDate
#' @param baseDate Used to define the first data of a data set in the form YYYY-MM-DD. Used to align data indeces
#' @return a data.frame with date, year, julien, and string representations of the data range
#' @keywords internal


define.dates= function(startDate, endDate, baseDate = NULL, splitDate = NULL, timestep = 1){

  bd = build.date(startDate, endDate)

  dates = seq.Date(as.Date(bd$startDate), as.Date(bd$endDate), 1)
  

  if(!is.null(splitDate)){
    pre = dates[as.Date(dates) < as.Date(splitDate)]
    post = dates[as.Date(dates) >= as.Date(splitDate)]

    dates = list(pre, post)
  } else { 
    dates = list(dates)
  }


  for(i in 1:length(dates)){
    dates[[i]] = data.frame(
      date   = dates[[i]],
      year   = as.numeric(format(dates[[i]], "%Y")),
      julien = as.numeric(format(dates[[i]], "%j"))
    )
  }
  
  if(!is.null(baseDate)) {

    base = c(baseDate, splitDate)

    for(i in 1:length(dates)){
      dates[[i]]$date.index  = as.numeric(dates[[i]]$date - as.Date(base[i])) 
      dates[[i]]$month.index = .numberOfMonths(dates[[i]]$date, base[i])
      dates[[i]] = merge(dates[[i]], .pentad(dates[[i]]$date,  base[i]))
    }
  }

  for(i in 1:length(dates)){
    dates[[i]]$string = format(dates[[i]]$date, format = "%Y%m%d")
  }

  if(length(dates) == 2){ dates = rbind(dates[[1]], dates[[2]])}

  if(class(dates) == 'list'){ dates = dates[[1]] }

  return(dates)
}



.pentad <- function(x,  baseDate) {

  all = data.frame(date = seq.Date(as.Date("1980-01-01"), Sys.Date(), 1))
  
  yday = as.POSIXlt(all$date)$yday + 1

  all$pentad = ceiling( (yday - .leap_year(all$date)*(yday > 59)) / 5 )
  
  all$pent_ind = cumsum(ifelse(all$pentad != c(-1,all$pentad[1:nrow(all)-1]), 1, 0))

  all     = all[all$date >= min(x),]
  all     = all[all$date <= max(x),]

}

.leap_year <- function(date) {
  if (is.numeric(date)) {
    year <- date
  } else {
    year <- year(date)
  }
  (year %% 4 == 0) & ((year %% 100 != 0) | (year %% 400 == 0))
}

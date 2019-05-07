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

#' @title Define Date Matrix
#' @description  **INTERNAL** From a user supplied start and end date gnerate the julien, string, and date representations of the data used in climateR
#' @param startDate character starting date for data retrieval in the form YYYY-MM-DD.
#' @param endDate character ending date for data retrieval in the form YYYY-MM-DD. Default is the startDate
#' @param baseDate Used to define the first data of a data set in the form YYYY-MM-DD. Used to align data indeces
#' @return a data.frame with date, year, julien, and string representations of the data range
#' @keywords internal


define.dates= function(startDate, endDate, baseDate = NULL, type = 'd'){

  type = NULL

  if(is.null(endDate)){ endDate = startDate}

  if(nchar(startDate) == 4){ startDate = paste0(startDate, "-01-01") }
  if(nchar(endDate)   == 4){ endDate = paste0(endDate,   "-12-31")  }

  days = .numberOfDays(as.Date(paste0(endDate, "-01")))

  if(nchar(startDate) == 7){ startDate = paste0(startDate, "-01") }
  if(nchar(endDate)   == 7){ endDate = paste0(endDate, "-", days) }

  if(nchar(startDate) == 10){ startDate = startDate }
  if(nchar(endDate)   == 10){ endDate   = endDate }

  dates = seq.Date(as.Date(startDate), as.Date(endDate), 1)

  dates = data.frame(
    date   = dates,
    year   = as.numeric(format(dates, "%Y")),
    julien = as.numeric(format(dates, "%j"))
  )

  if(!is.null(baseDate)) {
    dates$date.index = as.numeric(dates$date - as.Date(baseDate))
    dates$month.index = .numberOfMonths(dates$date, baseDate)
  }

  dates$string = format(dates$date, format = "%Y%m%d")

  return(dates)
}


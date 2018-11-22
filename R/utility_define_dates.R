
.numberOfDays <- function(date) {
  m <- format(date, format="%m")

  while (format(date, format="%m") == m) {
    date <- date + 1
  }

  return(as.integer(format(date - 1, format="%d")))
}

#' Title
#'
#' @param startDate
#' @param endDate
#'
#' @return
#' @export
#'
#' @examples
#'

define.dates= function(startDate, endDate, baseDate = NULL){

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

  if(!is.null(baseDate)) { dates$date.index = as.numeric(dates$date - as.Date(baseDate)) }
  dates$string = format(dates$date, format = "%Y%m%d")

  return(dates)
}


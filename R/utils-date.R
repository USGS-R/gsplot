
#' Returns the number of days the month of a given date
#' 
#' @param date a Date or POSIXct
#' @return numeric number of days in month of date
#' @keywords internal
ndays <- function(date) {
  if (length(date) != 1) {
    stop(length(date), " arguments passed to 'ndays' which requires 1")
  }
  last_days <- 28:31
  rev(last_days[which(!is.na(as.Date(paste(substr(date, 1, 8),
                                           last_days, sep = ''), '%Y-%m-%d')))])[1]
}

day_period <- function(period) UseMethod("day_period", object = period)
week_period <- function(period) UseMethod("week_period", object = period)
month_period <- function(period) UseMethod("month_period", object = period)
#' Returns the period padded to the quarter boundaries
#' 
#' @param period Date or POSIXct vector representing period of time
#' @return new period representing the padded range
#' @keywords internal
quarter_period <- function(period) UseMethod("quarter_period", object = period)
year_period <- function(period) UseMethod("year_period", object = period)
#' Returns the period padded to the water year boundaries
#' 
#' @param period Date or POSIXct vector representing period of time
#' @return new period representing the padded range
#' @keywords internal
wateryear_period <- function(period) UseMethod("wateryear_period", object = period)
#' Returns the period padded to the decade boundaries
#' 
#' @param period Date or POSIXct vector representing period of time
#' @return new period representing the padded range
#' @keywords internal
decade_period <- function(period) UseMethod("decade_period", object = period)

day_period.Date <- function(period) {
  return(c(as.Date(format(period[1], '%Y-%m-%d')),
           as.Date(format(period[2], '%Y-%m-%d'))))
}

day_period.POSIXt <- function(period) {
  return(posix_boundaries(day_period.Date(period)))
}

week_period.Date <- function(period) {
  return(c(as.Date(period[1]) - as.integer(format(as.Date(period[1]), '%w')), # 0 = sunday
    as.Date(period[2]) + (6 - as.integer(format(as.Date(period[2]), '%w'))))) # 6 = saturday
}

week_period.POSIXt <- function(period) {
  return(posix_boundaries(week_period.Date(period)))
}

month_period.Date <- function(period) {
  return(c(as.Date(format(period[1], '%Y-%m-01')),
    as.Date(format(period[2], paste0('%Y-%m-', ndays(period[2]))))))
}

month_period.POSIXt <- function(period) {
  return(posix_boundaries(month_period.Date(period)))
}

quarter_period.Date <- function(period) {
  qs <- quarters(period)
  formats <- lapply(qs, switch,
                    "Q1"=c("%Y-01-01", "%Y-03-31"),
                    "Q2"=c("%Y-04-01", "%Y-06-30"),
                    "Q3"=c("%Y-07-01", "%Y-09-30"),
                    "Q4"=c("%Y-10-01", "%Y-12-31"))
  return(c(as.Date(format(period[1], formats[[1]][1])), as.Date(format(period[2], formats[[2]][2]))))
}

quarter_period.POSIXt <- function(period) {
  return(posix_boundaries(quarter_period.Date(period)))
}

year_period.Date <- function(period) {
  return(c(as.Date(format(period[1], '%Y-01-01')),
    as.Date(format(period[2], '%Y-12-31'))))
}

year_period.POSIXt <- function(period) {
  return(posix_boundaries(year_period.Date(period)))
}

wateryear_period.Date <- function(period) {
  year <- sapply(period, function(instant) {
    offset <- ifelse(format(instant, "%m") < 10, 0, 1)
    return(as.integer(format(instant, "%Y")) + offset)
  })
  return(c(as.Date(paste0(year[1]-1, "-10-01")), as.Date(paste0(year[2], "-09-30"))))
}

wateryear_period.POSIXt <- function(period) {
  return(posix_boundaries(wateryear_period.Date(period)))
}

decade_period.Date <- function(period) {
  years <- c(floor(as.integer(format(period[1], "%Y"))/10)*10, 
             (ceiling(as.integer(format(period[2], "%Y"))/10)*10)-1)
  return(c(as.Date(paste0(years[1], "-01-01")), as.Date(paste0(years[2], "-12-31"))))
}

decade_period.POSIXt <- function(period) {
  return(posix_boundaries(decade_period.Date(period)))
}

posix_boundaries <- function(period) {
  return(c(as.POSIXct(format(period[1], '%Y-%m-%d 00:00:00')),
           as.POSIXct(format(period[2], '%Y-%m-%d 23:59:59'))))
}
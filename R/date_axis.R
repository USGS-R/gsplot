#' gsplot date axis
#' 
#' Special axis for date handling, including interval labelling.
#' 
#' @param lab.pos where should the label be positioned, centered on the "tick" or "interval".
#' @param tick.int interval in which ticks should be placed, alternative to defining at.
#' @param snap.to set the limits to coincide with temporal boundaries. Accepts "day", "week", "month", "quarter",
#' "year", "wateryear", "decade".
#' 
#'
#' @rdname date_axis
#' @export
#' @examples
#' x <- seq(as.Date("2013-01-22"), as.Date("2013-10-02"), "days")
#' y <- rnorm(length(x), 71, 19)
#' gs <- gsplot() %>%
#'    points(x, y) %>%
#'    date_axis(side=1, lab.pos="interval", tick.int="month", snap.to="year")
#' gs
date_axis <- function(object, ...) {
  override("gsplot", "date_axis", object, ...)
}

#'
#'
#' @export
date_axis.gsplot <- function(object, ..., side, lab.pos="tick", at=NULL, tick.int=NULL, snap.to=NULL) {
  if (exists("at") &&!is.null(at) && !is.null(tick.int)) {
    warning("cannot specify both at and tick.int, at will be ignored")
  }
  if (!is.null(tick.int)) {
    ticksAt <- lazy({
      # calc ticks
      limit <- lim(object, side)
      # TODO: handle -ly on tick.int
      seq(limit[1], limit[2], tick.int)
    })
  } else if (exists("at") && !is.null(at)) {
    ticksAt <- at
  } else {
    ticksAt <- NULL
  }

  labelsAt <- lazy({
    labels <- NULL
    limit <- lim(object, side)
    tick.loc <- object[[as.side_name(side)]][['axis']][['tick.loc']]
    
    if (is.null(tick.loc)) {
      main.ticks <- grid_axTicks(object, side)
    } else if (inherits(tick.loc, "lazy")) {
      main.ticks <- lazy_eval(tick.loc, data=list(object=object))
    } else {
      main.ticks <- tick.loc
    }
    
    if (lab.pos == "tick") {
      labels <- main.ticks
    } else if (lab.pos == "interval") {
      all.ints <- c(limit[1], main.ticks, limit[2])
      for (i in 2:length(all.ints)) {
        prev = all.ints[i-1]
        curr = all.ints[i]
        if (curr != prev) {
          label <- prev + (curr-prev) / 2
          if (is.null(labels)) labels <- label
          else labels <- c(labels, label)
        }
      }
    } else {
      stop("lab.pos must be \"tick\" or \"interval\"")
    }
    return(labels)
  })
  snapTo <- lazy({
    old.lim <- lim(object, side)
    # TODO: handle start.on.monday=TRUE use %u instead of %w and offset 1
    limit <- switch(snap.to,
           "day" = c(as.POSIXct(format(old.lim[1], '%Y-%m-%dT00:00:00')),
                     as.POSIXct(format(old.lim[2], '%Y-%m-%dT23:59:59'))),
           "week" = c(as.Date(old.lim[1]) - format(old.lim[1], '%w'), # 0 = sunday
                      as.Date(old.lim[2]) + (6 - format(old.lim[2], '%w'))), # 6 = saturday
           "month" = c(as.Date(format(old.lim[1]), '%Y-%m-01'),
                      as.Date(format(old.lim[2]), paste0('%Y-%m-', ndays(old.lim[2])))),
           "quarter" = quarter_period(old.lim),
           "year" = c(as.Date(format(old.lim[1], '%Y-01-01')),
                      as.Date(format(old.lim[2], '%Y-12-31'))),
           "wateryear" = wateryear_period(old.lim),
           "decade" = decade_period(old.lim),
           old.lim # default
    )
  })

  object <- axis(object, ..., side, at=ticksAt, labels=FALSE)
  object <- axis(object, ..., side, at=labelsAt, tick.loc=ticksAt, tick=FALSE, append=TRUE)
  for (side_name in as.side_name(side)) {
    object[[side_name]][["snap.to"]] <- snapTo
  }
  return(object)
}


#'
#'
#' @export
date_axis.default <- function(side, lab.pos="tick", tick.int=NULL, snap.to=NULL, ...) {
  warning("date_axis is not implemented for base graphics plots")
  return()
}

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

#' Returns the period padded to the quarter boundaries
#' 
#' @param period Date or POSIXct vector representing period of time
#' @return new period representing the padded range
#' @keywords internal
quarter_period <- function(period) {
  qs <- quarters(period)
  formats <- lapply(qs, switch,
                   "Q1"=c("%Y-01-01", "%Y-03-31"),
                   "Q2"=c("%Y-04-01", "%Y-06-30"),
                   "Q3"=c("%Y-07-01", "%Y-09-30"),
                   "Q4"=c("%Y-10-01", "%Y-12-31"))
  return(c(as.Date(format(period[1], formats[[1]][1])), as.Date(format(period[2], formats[[2]][2]))))
}

#' Returns the period padded to the water year boundaries
#' 
#' @param period Date or POSIXct vector representing period of time
#' @return new period representing the padded range
#' @keywords internal
wateryear_period <- function(period) {
  year <- sapply(period, function(instant) {
    offset <- ifelse(format(instant, "%m") < 10, 0, 1)
    return(as.integer(format(instant, "%Y")) + offset)
  })
  return(c(as.Date(paste0(year[1]-1, "-10-01")), as.Date(paste0(year[2], "-09-30"))))
}

#' Returns the period padded to the decade boundaries
#' 
#' @param period Date or POSIXct vector representing period of time
#' @return new period representing the padded range
#' @keywords internal
decade_period <- function(period) {
  years <- c(floor(as.integer(format(period[1], "%Y"))/10)*10, 
             (ceiling(as.integer(format(period[2], "%Y"))/10)*10)-1)
  return(c(as.Date(paste0(years[1], "-01-01")), as.Date(paste0(years[2], "-12-31"))))
}
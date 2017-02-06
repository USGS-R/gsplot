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
#' x <- seq(as.Date("2013-01-22"), as.Date("2013-12-02"), "days")
#' y <- rnorm(length(x), 71, 19)
#' gs <- gsplot() %>%
#'    points(x, y) %>%
#'    date_axis(lab.pos="interval", tick.int="monthly", snap.to="year")
#' gs
date_axis <- function(object, ...) {
  override("gsplot", "date_axis", object, ...)
}

date_axis.gsplot <- function(object, ..., lab.pos="tick", tick.int=NULL, snap.to=NULL) {
  if (!is.null(at) && !is.null(tick.int)) {
    warning("cannot specify both at and tick.int, at will be ignored")
  }
  if (!is.null(tick.int)) {
    ticksAt <- lazy(function() {
      # calc ticks
      limit <- lim(object, side)[[1]] # limits should be shared
      # TODO: handle -ly on tick.int
      seq(limit[1], limit[2], tick.int)
    })
  } else if (!is.null(at)) {
    ticksAt <- at
  } else {
    ticksAt <- NULL
  }

  labelsAt <- lazy(function(){
    labels <- NULL
    limit <- lim(object, side)[[1]]
    if (lab.pos == "tick") {
      labels <- ticksAt
    } else if (lab.pos == "interval") {
      prev = limit[1]
      for (intBnd in c(ticksAt, limit[2])) {
        if (intBnd != prev) {
          labels <- c(labels, prev + (intBnd-prev)/2)
        }
        prev = intBnd
      }
    } else {
      stop("lab.pos must be \"tick\" or \"interval\"")
    }
    return(labels)
  })
  snapTo <- lazy(function() {
    old.lim <- lim(object, side)[[1]]
    # TODO: handle start.on.monday=TRUE use %u instead of %w and offset 1
    limit <- switch(snap.to,
           "day" = c(as.POSIXct(format(old.lim[1]), '%Y-%m-%dT00:00:00'), 
                     as.POSIXct(format(old.lim[2], '%Y-%m-%dT23:59:59'))),
           "week" = c(as.Date(old.lim[1]) - format(old.lim[1], '%w'), # 0 = sunday
                      as.Date(old.lim[2]) + (6 - format(old.lim[2], '%w'))), # 6 = saturday
           "month" = c(as.Date(format(old.lim[1]), '%Y-%m-01'),
                      as.Date(format(old.lim[2]), paste0('%Y-%m-', ndays(old.lim[2])))), 
           "quarter" = c(), # NOT YET IMPLEMENTED
           "year" = c(as.Date(format(old.lim[1], '%Y-01-01')),
                      as.Date(format(old.lim[2], '%Y-12-31'))),
           "wateryear" = c(), # NOT YET IMPLEMENTED
           "decade" = c(), # NOT YET IMPLEMENTED
           old.lim # default
    )
  })
  # can we lazy eval 'at'
  object <- axis(object, ..., at=ticksAt, labels=FALSE)
  object <- axis(object, ..., at=labelsAt, tick=FALSE, append=TRUE)
}

date_axis.default <- function(lab.pos="tick", tick.int=NULL, snap.to=NULL, ...) {
  warning("date_axis is not implemented for base graphics plots")
  return()
}

ndays <- function(date) {
  last_days <- 28:31
  rev(last_days[which(!is.na(as.Date(paste(substr(date, 1, 8),
                                           last_days, sep = ''), '%Y-%m-%d')))])[1]
}

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
date_axis.gsplot <- function(object, ..., side, lab.pos="tick", at=NULL, tick.int=NULL, snap.to="day") {
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
    
    if (is.null(ticksAt)) {
      main.ticks <- grid_axTicks(object, side)
    } else if (inherits(ticksAt, "lazy")) {
      main.ticks <- lazy_eval(ticksAt, data=list(object=object))
    } else {
      main.ticks <- ticksAt
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
           "day" = day_period(old.lim),
           "week" = week_period(old.lim),
           "month" = month_period(old.lim),
           "quarter" = quarter_period(old.lim),
           "year" = year_period(old.lim),
           "wateryear" = wateryear_period(old.lim),
           "decade" = decade_period(old.lim),
           old.lim # default
    )
  })

  object <- axis(object, ..., side, at=ticksAt, labels=FALSE)
  object <- axis(object, ..., side, at=labelsAt, tick=FALSE, append=TRUE)
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


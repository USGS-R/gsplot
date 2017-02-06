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
  
  # can we lazy eval 'at'
  object <- axis(object, ..., at=seq(year[1], year[2], "months"), labels=FALSE)
  object <- axis(object, ..., at=seq(year[1], year[2], "months")+15, tick=FALSE, append=TRUE)
}

date_axis.default <- function(lab.pos="tick", tick.int=NULL, snap.to=NULL, ...) {
  warning("date_axis is not implemented for base graphics plots")
  return()
}

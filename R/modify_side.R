#' modify a side to update it with new arguments
#' 
#' @param object gsplot object
#' @param side sides to modify with extra parameters
#' @param \dots user arguments to passed through
#' 
#' @keywords internal
#' @examples
#' gs <- gsplot() %>%
#'    lines(c(0,10), c(0,10))
#' gs <- gsplot:::modify_side(gs, c(1,2), xlim=c(0,12))
#' 
modify_side <- function(object, side, ...) {
  sides <- sides(object, side)
  sideNames <- names(sides)
  allArgs <- list(...)
  for (sideName in sideNames) {
    sideNum <- as.side(sideName)
    sideArgs <- which(side_cares_about_this(names(allArgs), sideNum))
    if (any(sideArgs)) {
      addTheseToSide <- allArgs[sideArgs]
      thisSide <- sides[[sideName]]
      thisSide <- set_side_lim(allArgs, thisSide, sideNum)
      thisSide <- set_side_log(allArgs, thisSide, sideNum)
      thisSide <- set_side_lab(allArgs, thisSide, sideNum)
      object[[sideName]] <- thisSide
    }
  }
  return(object)
}



side_cares_about_this <- function(arg.name, side) {
  #c('log','ylim','xlim','xlab','ylab')
  return((side %% 2 == 1 & arg.name %in% c('log', 'xlim', 'xlab')) |
           (side %% 2 == 0 & arg.name %in% c('log', 'ylim', 'ylab')))
}

#' modify a side to update it with new arguments
#' 
#' @param object gsplot object
#' @param args a list of arguments
#' @param side sides to modify with extra parameters
#' 
#' @keywords internal
#' @examples
#' gs <- gsplot() %>%
#'    lines(c(0,10), c(0,10))
#' gs <- gsplot:::modify_side(gs, c(1,2), xlim=c(0,12))
#' 
modify_side <- function(object, args, side) {
  object <- append_sides(object, side, on.exists="skip")
  sides <- sides(object, side)
  sideNames <- names(sides)
  for (sideName in sideNames) {
    sideNum <- as.side(sideName)
    thisSide <- sides[[sideName]]
    thisSide <- set_side_lim(args, thisSide, sideNum)
    thisSide <- set_side_log(args, thisSide, sideNum)
    thisSide <- set_side_lab(args, thisSide, sideNum)
    thisSide <- set_side_axes(args, thisSide, sideNum)
    object[[sideName]] <- thisSide
  }
  return(object)
}
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
#' gs <- gsplot:::modify_side(gs, args = list(x=c(1,2), xlim=c(0,12)), side=1)
#' 
modify_side <- function(object, args, side) {
  
  incoming.side.names <- as.side_name(side)
  
  new.sides <- !incoming.side.names %in% side_names(object)
  
  if (any(new.sides)){
    for (side.name in incoming.side.names[new.sides]){
      object <- add_new_side(object, side.name)
    }
  }

  sides <- sides(object, side)
  sideNames <- names(sides)
  for (sideName in sideNames) {
    sideNum <- as.side(sideName)
    thisSide <- sides[[sideName]]
    
    if("side.3" == sideName & length(views_with_side(object, 3)) == 0 & "side.1" %in% names(object)){
      thisSide$log <- object$side.1$log
    } else if ("side.4" == sideName & length(views_with_side(object, 4)) == 0 & "side.2" %in% names(object)){
      thisSide$log <- object$side.2$log
    } else if ("side.1" == sideName & length(views_with_side(object, 1)) == 0 & "side.3" %in% names(object)){
      thisSide$log <- object$side.3$log
    } else if ("side.2" == sideName & length(views_with_side(object, 2)) == 0 & "side.4" %in% names(object)){
      thisSide$log <- object$side.4$log
    }
    
    thisSide <- set_side_lim(args, thisSide, sideNum)
    thisSide <- set_side_log(args, thisSide, sideNum)
    thisSide <- set_side_lab(args, thisSide, sideNum)
    thisSide <- set_side_axes(args, thisSide, sideNum)
    

    object[[sideName]] <- thisSide
  }
  
  return(object)
}
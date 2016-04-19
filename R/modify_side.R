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
  sideNames <- as.side_name(set_sides(side))
  allArgs <- list(...)
  sides <- lapply(side, function(sideNum) {
    sideArgs <- which(side_cares_about_this(names(allArgs), sideNum))
    if (any(sideArgs)) {
      addTheseToSide <- allArgs[[sideArgs]]
    }
  })
#   vew.n.sde <- gsplot[which_sides(gsplot)]
#   vew.n.sde <- append_sides(vew.n.sde, add_sides)
#   to_draw <- setNames(list(c(tail.gs[['arguments']], legend.name=tail.gs[['gs.config']][['legend.name']])), tail.nm)
#   view.name <- as.view_name(c(add_sides[1],add_sides[2]))
#   sides <- sides(vew.n.sde)
#   named.view <- list(to_draw) %>% setNames(view.name)
#   sides <- set_side_lim(named.view, sides)
#   sides <- set_side_log(named.view, sides)
#   sides <- set_side_lab(named.view, sides)
#   to_draw[[1]] <- remove_field(to_draw[[1]], param=c('log','ylim','xlim','xlab','ylab'))
  
#  vew.n.sde <- append_replace(vew.n.sde, sides)
  return(object)
}

side_cares_about_this <- function(arg.name, side) {
  #c('log','ylim','xlim','xlab','ylab')
  return((side %% 2 == 1 & arg.name %in% c('log', 'xlim', 'xlab')) |
           (side %% 2 == 0 & arg.name %in% c('log', 'ylim', 'ylab')))
}

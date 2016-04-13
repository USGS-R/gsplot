
#' get the indices of the gsplot object for where the views are
#' 
#' @param gsplot a gsplot object
#' @return a vector of ints
#' @keywords internal
which_views <- function(gsplot){
  grep('view.', names(gsplot))
}

#' names of the views in gsplot object
#' 
#' @param gsplot a gsplot object
#' @return a character vector of view names
#' @keywords internal
view_names <- function(gsplot){
  names(views(gsplot))
}

#' views in the gsplot object
#' 
#' @param gsplot a gsplot object
#' @return a subset of the gsplot object that contains only views
#' @keywords internal
views <- function(gsplot){
  gsplot[which_views(gsplot)]
}

#' non-views in the gsplot object
#' 
#' @param gsplot a gsplot object
#' @return a subset of the gsplot object that contains only non-views
#' @keywords internal
non_views <- function(gsplot, include.sides = TRUE){
  non.views <- gsplot
  non.views[which_views(non.views)] <- NULL
  if (!include.sides)
    non.views[which_sides(non.views)] <- NULL
  return(non.views)
}

#' convert sides vector into view name
#' 
#' @param sides a vector of sides (if only length 1, appended via \code{set_sides})
#' @return a vector of view names
#' @keywords internal
as.view_name <- function(sides){
  paste0('view.', paste(set_sides(sides), collapse='.'))
}


#' find views with a certain side
#' 
#' @param views a gsplot object or subset of gsplot object containing views
#' @param side a single side value to compare to the views
#' @return indices for views that have this side
#' @keywords internal
views_with_side <- function(views, side){
  if(length(side) > 1)
    stop('side can only be length of 1')
  with.side = lapply(views, function(x) any(x[['window']][['side']] %in% side))
  view.match = unname(unlist(with.side[which_views(views)]))
  if (is.null(view.match) || !any(view.match))
    return(NULL)
  else
    return(which(view.match))
}

true_in_any <- function(views, field){
  values <- strip_pts(views, field) #///// WIP
}

#' get the side from a view for an axis
#' 
#' @param views a gsplot object or subset of gsplot object containing views
#' @param view_i an index for views to be used
#' @param axis 'y' or 'x'
#' @return the side vector
#' @keywords internal
get_view_side <- function(views, view_i, axis){
  i = which_views(views)[view_i]
  sides <- views[[i]][['window']][['side']]
  if (axis=='y')
    return(sides[which(sides %% 2 == 0)])
  else if (axis=='x')
    return(sides[which(sides %% 2 != 0)])
  else
    stop('view side undefined for ',axis)
}

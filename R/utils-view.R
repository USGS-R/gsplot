
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
  view.match <- which(grepl(paste0('.',side), names(views(views))))
  return(view.match)
}

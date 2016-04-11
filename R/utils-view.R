
#' get the indices of the gsplot object for where the views are
#' 
#' @param gsplot a gsplot object
#' @return a vector of ints
which_views <- function(gsplot){
  grep('view.', names(gsplot))
}


view_names <- function(gsplot){
  names(views(gsplot))
}

views <- function(gsplot){
  gsplot[which_views(gsplot)]
}

non_views <- function(gsplot, include.sides = TRUE){
  non.views <- gsplot
  non.views[which_views(non.views)] <- NULL
  if (!include.sides)
    non.views[which_sides(non.views)] <- NULL
  return(non.views)
}

as.view_name <- function(sides){
  paste0('view.', paste(sides, collapse='.'))
}


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

get_view_side <- function(views, view_i, param){
  i = which_views(views)[view_i]
  sides <- views[[i]][['window']][['side']]
  if (param=='y')
    return(sides[which(sides %% 2 == 0)])
  else if (param=='x')
    return(sides[which(sides %% 2 != 0)])
  else
    stop('view side undefined for ',param)
}

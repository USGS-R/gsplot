#' calc_views
#' 
#' calc_views
#' 
#' @param gsplot a gsplot object
#' @export
calc_views <- function(gsplot){
  
  views <- group_views(gsplot)
  
  views <- calc_view_lims(views)
  
  return(views)
}


group_views <- function(gsplot){
  unique_sides <- unique(lapply(gsplot, function(x) x[['gs.config']][['side']]))
  unique_sides <- unique_sides[!sapply(unique_sides, is.null)]
  views <- rep(list(view=c()),length(unique_sides))
  
  for (i in 1:length(unique_sides)){
    views[[i]][['gs.config']][['side']] = unique_sides[[i]]
  }
  
  for (i in seq_len(length(gsplot))){
    draw_sides <- gsplot[[i]][['gs.config']][['side']]
    if (!is.null(draw_sides)){
      view_i <- which(sapply(unique_sides, function(x) x[1] == draw_sides[1] & x[2] == draw_sides[2]))
    } else {
      view_i = 1 
    }
    to_draw <- setNames(list(gsplot[[i]][['arguments']]), names(gsplot[i]))
    views[[view_i]] <- append(views[[view_i]], to_draw)
  }
  
  return(views)
}

calc_view_lims <- function(views){
  
  for (i in 1:length(views)){
    view <- views[[i]]
    x <- lapply(view, var='x', function(list, var) strip_pts(list,var))
    y <- lapply(view, var='y', function(list, var) strip_pts(list,var))
    xlim <- lims_from_list(x)
    ylim <- lims_from_list(y)
    views[[i]]$usr <- c(usr_from_lim(xlim, type=par()$xaxs), usr_from_lim(ylim, type=par()$yaxs))
  }
  return(views)
}

strip_pts <- function(list, var){
  if (var %in% names(list))
    list[[var]]
  else
    NA
}
lims_from_list <- function(list){
  c(min(sapply(list, min),na.rm=TRUE), max(sapply(list, max),na.rm=TRUE))
}

usr_from_lim <- function(lim, type = 'i', log=FALSE){
  
  if (log)
    stop('log = TRUE not currently supported')
  usr <- switch (type,
    i = lim,
    r = c(lim[1]-0.04*lim[1], lim[2]+0.04*lim[2])
  )
  if (diff(usr) == 0){
    usr <- c(usr[1]-0.5, usr[2]+0.5)
  }
  return(usr)
}
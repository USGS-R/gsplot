#' calc_view
#' 
#' calc_view
#'
#'
#' @param gsplot object
#' @export
#' @importFrom graphics par
#' @keywords internal
calc_views <- function(gsplot){
  
  views <- group_views(gsplot)
  
  views <- set_view_log(views)

  views <- set_view_lim(views)
  
  views <- set_view_lab(views)
  
  views <- set_view_order(views)
  
  return(views)
}


group_views <- function(gsplot){
  unique_sides <- unique(lapply(gsplot, function(x) x[['gs.config']][['side']]))
  unique_sides <- unique_sides[!sapply(unique_sides, is.null)]
  views <- rep(list(view=c()),length(unique_sides))
  
  for (i in seq_len(length(unique_sides))){
    views[[i]][['window']][['side']] = unique_sides[[i]]
  }
  
  for (i in seq_len(length(gsplot))){
    draw_sides <- gsplot[[i]][['gs.config']][['side']]
    if (!is.null(draw_sides)){
      # // to do: verify sides are in order: x then y
      view_i <- which(sapply(unique_sides, function(x) x[1] == draw_sides[1] & x[2] == draw_sides[2]))
      to_draw <- setNames(list(gsplot[[i]][['arguments']]), names(gsplot[i]))
      views[[view_i]] <- append(views[[view_i]], to_draw)
    } else {
      # // if field isn't associated with a side(s), it is moved up to top level (e.g., legend)
      views[[names(gsplot[i])]] <- gsplot[[i]]
    }
  }
  
  return(views)
}

set_view_list <- function(views, var, na.action=NA, remove=TRUE){
  view_i <- which(names(views) %in% "view")
  for (i in view_i){
    values <- do.call(rbind, lapply(views[[i]], function(x) strip_pts(x, var)))
    val.i <- which(!is.na(values)) # which row to use
    if (length(val.i) == 0){
      values = na.action
    } else {
      if (length(unique(rownames(values[val.i]))) > 1)
        warning('for view ', i,', more than one ',var,' specified. ', unique(rownames(values[val.i])))
      values <- values[val.i]
    }
    if (remove)
      views[[i]] <- lapply(views[[i]], function(x) remove_field(x, var))
    
    views[[i]][['window']][[var]] <- values
  }
  
  return(views)
}

set_view_log <- function(views){
  set_view_list(views, var = 'log', na.action="")
}

set_view_lab <- function(views){
  views <- set_view_list(views, var = 'ylab', na.action="") %>% 
    set_view_list(var = 'xlab', na.action="")
}


set_view_lim <- function(views){
  views <- set_view_list(views, var = 'xlim', na.action=NA) %>% 
    set_view_list(views, var = 'ylim', na.action=NA)
  
  data <- list(y=summarize_args(views,c('y','y1','y0'),ignore=c('window','gs.config')), 
               x=summarize_args(views,c('x','x1','x0'),ignore=c('window','gs.config')))

  for(var in c('y','x')){
    for (i in names(data[[var]])){
      lim.name <- paste0(var,'lim')
      view.side <- get_view_side(views, as.numeric(i), var)
      match.side <- as.character(views_with_side(views, view.side))
      data.lim <- range(data[[var]][[match.side]][is.finite(data[[var]][[match.side]])])
      usr.lim <- views[[as.numeric(i)]][['window']][[lim.name]][1:2]
      views[[as.numeric(i)]][['window']][[lim.name]] <- data.lim
      views[[as.numeric(i)]][['window']][[lim.name]][!is.na(usr.lim)] <- usr.lim[!is.na(usr.lim)]
  
    }
  }

  return(views)
}

views_with_side <- function(views, side){
  with.side = lapply(views, function(x) any(x[['window']][['side']] %in% side))
  unname(which(unlist(with.side[names(with.side) == 'view'])))
}

get_view_side <- function(views, view_i, var){
  i = which(names(views) %in% 'view')[view_i]
  sides <- views[[i]][['window']][['side']]
  if (var=='y')
    return(sides[which(sides %% 2 == 0)])
  else if (var=='x')
    return(sides[which(sides %% 2 != 0)])
  else
    stop('view side undefined for ',var)
}

summarize_args <- function(views, var, na.action,ignore='gs.config'){
  
  view_i <- which(names(views) %in% "view")
  values <- list()
  for (i in view_i){
    x <- views[[i]][!names(views[[i]]) %in% ignore]
    valStuff <- lapply(x, function(x) strip_pts(x, var))
    values[[i]] <- unname(do.call(c,valStuff))

  }
  names(values) <- view_i
  return(values)
}

remove_field <- function(list, var){
 
  for (v in var){
    if (v %in% names(list))
      list[[v]] <- NULL
  }
  return(list)
}

strip_pts <- function(list, var){
  out <- c()
  
  for (v in var){
    if (v %in% names(list))
      out <- append(out, list[[v]])
    else
      out <- append(out, NA)
  }
  return(out)
}

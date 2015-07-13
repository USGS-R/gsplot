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
  
  #Start enforcing some order. Background color has to go in back...I'd also put grid in back:
  if("legend" %in% names(views)){
    legendChunk <- views$legend
    views <- views[-which(names(views) %in% "legend")]
  }
   
  
  if("grid" %in% names(do.call(c, unname(views)))){
    newView <- list()
    for(i in 1:length(views)){
      subView <- views[[i]]
      if("grid" %in% names(subView)){
        gridView <- subView$grid
        otherViews <- subView[-which(names(subView) %in% "grid")]
        subView <- append(list(grid=gridView), otherViews) 
      }
      newView <- append(newView, list(view=subView))
    }
    views <- newView

  }
  
  if("bgCol" %in% names(do.call(c, unname(views)))){
    newView <- list()
    for(i in 1:length(views)){
      subView <- views[[i]]
      if("bgCol" %in% names(subView)){
        bgColView <- subView$bgCol
        otherViews <- subView[-which(names(subView) %in% "bgCol")]
        subView <- append(list(bgCol=bgColView), otherViews)
      }
      newView <- append(newView, list(view=subView))
    }
    views <- newView
  }
  
  if("legend" %in% names(views)){
    views <- append(views, list(legend=legendChunk))
  }
  
  return(views)
}


group_views <- function(gsplot){
  unique_sides <- unique(lapply(gsplot, function(x) x[['gs.config']][['side']]))
  unique_sides <- unique_sides[!sapply(unique_sides, is.null)]
  views <- rep(list(view=c()),length(unique_sides))
  
  for (i in seq_len(length(unique_sides))){
    views[[i]][['gs.config']][['side']] = unique_sides[[i]]
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
    if (length(val.i) > 1){
      warning('for view ', i,', more than one ',var,' specified. Using last')
      values <- values[val.i[1], ]
    } else if (length(val.i) == 0){
      values = na.action
    } else 
      values <- values[val.i, ]
    if (remove)
      views[[i]] <- lapply(views[[i]], function(x) remove_field(x, var))
    
    views[[i]][['gs.config']][[var]] <- values
  }
  
  return(views)
}

set_view_log <- function(views){
  set_view_list(views, var = 'log', na.action="")
}

set_view_lab <- function(views){
  views <- set_view_list(views, var = 'ylab', na.action="")
  set_view_list(views, var = 'xlab', na.action="")
}


set_view_lim <- function(views){
  views <- set_view_list(views, var = 'xlim', na.action=NA)
  views <- set_view_list(views, var = 'ylim', na.action=NA)
  
  data <- list(y=summarize_args(views,c('y','y1','y0')), x=summarize_args(views,c('x','x1','x0')))
  var <- 'y'
  for (i in names(data[[var]])){
    lim.name <- paste0(var,'lim')
    data.lim <- range(data[[var]][[i]], na.rm = T, na.action = NA)
    usr.lim <- views[[as.numeric(i)]][['gs.config']][[lim.name]][1:2]
    views[[as.numeric(i)]][['gs.config']][[lim.name]] <- ifelse(is.na(usr.lim), data.lim, usr.lim)
  }
  var <- 'x'
  for (i in names(data[[var]])){
    lim.name <- paste0(var,'lim')
    data.lim <- range(data[[var]][[i]], na.rm = T, na.action = NA)
    usr.lim <- views[[as.numeric(i)]][['gs.config']][[lim.name]][1:2]
    views[[as.numeric(i)]][['gs.config']][[lim.name]] <- ifelse(is.na(usr.lim), data.lim, usr.lim)
  }
  return(views)
}

summarize_args <- function(views, var, na.action,ignore='gs.config'){
  
  view_i <- which(names(views) %in% "view")
  values <- list()
  for (i in view_i){
    x <- views[[i]][!names(views[[i]]) %in% ignore]
    values[[i]] <- as.numeric(unname(unlist(sapply(x, function(x) strip_pts(x, var)))))
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
      out <- c(out, list[[v]])
    else
      out <- c(out, NA)
  }
  return(out)
}

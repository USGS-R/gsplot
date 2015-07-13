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
  
  logs <- calc_view_logs(views)
  
  views <- add_view_logs(views, logs)
  
  usrs <- calc_view_usr(views)
  
  labs <- calc_view_labs(views)
  
  views <- add_view_usr(views, usrs)
  
  views <- add_view_labs(views, labs)
  
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

add_view_usr <- function(views, usrs){
  view_i <- which(names(views) %in% 'view')
  for (i in view_i){
    sides <- views[[i]][['gs.config']][['side']]
    views[[i]][['gs.config']][['usr']] <- c(usrs[[sides[1]]][[1]], usrs[[sides[2]]][[1]])
  }
  return(views)
}

add_view_logs <- function(views, logs){
  view_i <- which(names(views) %in% 'view')
  for (i in view_i){
    sides <- views[[i]][['gs.config']][['side']]
    views[[i]][['gs.config']][['xlog']] <- logs[[sides[1]]][[1]]
    views[[i]][['gs.config']][['ylog']] <- logs[[sides[2]]][[1]]
    # // strip out log 
    non_config <- which(!names(views[[i]]) %in% 'gs.config')
    for (j in non_config){
      views[[i]][[j]][['log']] <- NULL
    }
  }


  
  # // to do: remove logs from all non-gs.config fields 
  return(views)
}

add_view_labs <- function(views, labs){
  view_i <- which(names(views) %in% 'view')
  for (i in view_i){
    sides <- views[[i]][['gs.config']][['side']]
    views[[i]][['gs.config']][['xlab']] <- labs[[sides[1]]]
    views[[i]][['gs.config']][['ylab']] <- labs[[sides[2]]]
  }
  return(views)
}

calc_view_usr <- function(views){
  
  unique_sides <- unique(unlist(lapply(views,function(x)x[['gs.config']][['side']])))
  sides <- list()
  for (side in unique_sides){
    
    view_i <- which(unlist(lapply(views,function(x)any(x[['gs.config']][['side']]==side))))
    
    # collapse these into a single list of components that reference this side
    side_components <- do.call(c,views[view_i]) 
    
    if ((side %% 2) == 0){
      # is y 
      if (any(unname(unlist(lapply(side_components, names))) %in% c('y','y1','y0'))){
        lims <- lims_from_list(lapply(side_components, var=c('y','y1','y0'), function(list, var) strip_pts(list,var)))
        client_lims <- lims_from_client(side_components, var='ylim', side)
        lims[!is.na(client_lims)] <- client_lims[!is.na(client_lims)]
      } else {
        dataViews <- views[!(names(views) %in% "legend")]
        dataViews <- do.call(c, unname(dataViews))
        dataViews <- dataViews[!names(dataViews) %in% c("axis","gs.config")]
        dataViews <- do.call(c, unname(dataViews))
        yData <- do.call(c, unname(dataViews[names(dataViews) %in% c('y','y1','y0')]))

        lims <- range(yData,na.rm=TRUE)
        client_lims <- dataViews[grep("^ylim$", names(dataViews))]
        if(length(client_lims) > 0){
          client_lims <- client_lims[[1]]
          lims[!is.na(client_lims)] <- client_lims[!is.na(client_lims)]
        }
      }
      
      usr <- usr_from_lim(lims, type=par()$yaxs, log = side_components$view.gs.config$ylog)
    } else {
      # is x
      if(any(unname(unlist(lapply(side_components, names))) %in% c('x','x1','x0'))){
        lims <- lims_from_list(lapply(side_components, var=c('x','x1','x0'), function(list, var) strip_pts(list,var)))
        client_lims <- lims_from_client(side_components, var='xlim', side)
        lims[!is.na(client_lims)] <- client_lims[!is.na(client_lims)]
      } else {
        dataViews <- views[!(names(views) %in% "legend")]
        dataViews <- do.call(c, unname(dataViews))
        dataViews <- dataViews[!names(dataViews) %in% c("axis","gs.config")]
        dataViews <- do.call(c, unname(dataViews))
        xData <- do.call(c, unname(dataViews[names(dataViews) %in% c('x','x1','x0')]))
        
        lims <- range(xData,na.rm=TRUE)
        client_lims <- dataViews[grep("^xlim$", names(dataViews))]
        if(length(client_lims) > 0){
          client_lims <- client_lims[[1]]
          lims[!is.na(client_lims)] <- client_lims[!is.na(client_lims)]
        }
      }
      
      usr <- usr_from_lim(lims, type=par()$xaxs, log = side_components$view.gs.config$xlog)
    }
    sides[[side]] = list(usr=usr)
    
  }
  names(sides) <- unique_sides
  return(sides)
}

calc_view_logs <- function(views){
  
  #// to do: refactor to eliminate duplicated comb_view methods (like this on and the usr one)
  unique_sides <- unique(unlist(lapply(views,function(x)x[['gs.config']][['side']])))
  logs <- list()
  for (side in unique_sides){
    
    view_i <- which(unlist(lapply(views,function(x)any(x[['gs.config']][['side']]==side))))
    
    # collapse these into a single list of components that reference this side
    side_components <- do.call(c,views[view_i]) 
    
    var = 'log'
    if ((side %% 2) == 0){
      # is y 
      
      log <- logs_from_client(side_components, var=var,side=side,match='y')
    } else {
      # is x
      log <- logs_from_client(side_components, var=var,side=side,match='x')
    }
    
    logs[[side]] = log
    
  }
  names(logs) <- unique_sides
  return(logs)
}

calc_view_labs <- function(views){
  
  #// to do: refactor to eliminate duplicated comb_view methods (like this on and the usr one)
  unique_sides <- unique(unlist(lapply(views,function(x)x[['gs.config']][['side']])))
  labels <- list()
  for (side in unique_sides){
    
    view_i <- which(unlist(lapply(views,function(x)any(x[['gs.config']][['side']]==side))))
    
    # collapse these into a single list of components that reference this side
    side_components <- do.call(c,views[view_i]) 

    if ((side %% 2) == 0){
      # is y 
      var = 'ylab'
      lab <- labs_from_client(side_components, var=var,side=side)
    } else {
      # is x
      var = 'xlab'
      lab <- labs_from_client(side_components, var=var,side=side)
    }

    labels[[side]] = lab
    
  }
  names(labels) <- unique_sides
  return(labels)
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

lims_from_client <- function(list, var, side){
  client_lims <- lapply(list, var=var, function(list, var) strip_pts(list,var))
  client_lims <- client_lims[!is.na(client_lims)]
  if (length(client_lims) > 1)
    warning('for side ', side,', more than one ',var,' specified. Using last')
  
  if (length(client_lims) == 0)
    client_lims <- list(c(NA,NA))
  
  return(client_lims[[length(client_lims)]])
}

labs_from_client <- function(list,var,side){
  client_labs <- lapply(list, var=var, function(list, var) strip_pts(list,var))
  client_labs <- client_labs[!is.na(client_labs)]
  if (length(client_labs) > 1)
    warning('for side ', side,', more than one ',var,' specified. Using last')
  
  if (length(client_labs) == 0)
    client_labs <- list(NA)
  return(client_labs[[length(client_labs)]])
}

logs_from_client <- function(list,var,side, match){
  client_logs <- lapply(list, var=var, function(list, var) strip_pts(list,var))
  client_logs <- client_logs[!is.na(client_logs)]
  if (length(client_logs) > 1)
    warning('for side ', side,', more than one ',var,' specified. Using last')
  
  if (length(client_logs) == 0)
    client_logs <- list(NA)
  return(client_logs[[length(client_logs)]] == match)
}


lims_from_list <- function(list){

  range(list,na.rm = T, na.action = NA)
}

usr_from_lim <- function(lim, type = 'i', log=FALSE){
  
  if (log)
    lim <- log10(lim)
  usr <- switch (type,
    i = lim,
    r = c(lim[1]-0.04*diff(lim), lim[2]+0.04*diff(lim))
  )
  if (diff(usr) < 1){
    usr <- c(usr[1]-0.5, usr[2]+0.5)
  }
  return(usr)
}
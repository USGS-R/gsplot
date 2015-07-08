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
  
  usrs <- calc_view_usr(views)
  
  labs <- calc_view_labs(views)
  
  views <- add_view_usr(views, usrs)
  
  views <- add_view_labs(views, labs)
  
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
      if(length(grep("^y$", unlist(lapply(side_components, names)))) > 0){
        lims <- lims_from_list(lapply(side_components, var='y', function(list, var) strip_pts(list,var)))
        client_lims <- lims_from_client(side_components, var='ylim', side)
        lims[!is.na(client_lims)] <- client_lims[!is.na(client_lims)]
      } else {
        dataViews <- views[!(names(views) %in% "legend")]
        dataViews <- do.call(c, unname(dataViews))
        dataViews <- dataViews[!names(dataViews) %in% c("axis","gs.config")]
        dataViews <- do.call(c, unname(dataViews))
        yData <- do.call(c, unname(dataViews[grep("^y$", names(dataViews))]))

        lims <- range(yData,na.rm=TRUE)
        client_lims <- dataViews[grep("^ylim$", names(dataViews))]
        if(length(client_lims) > 0){
          client_lims <- client_lims[[1]]
          lims[!is.na(client_lims)] <- client_lims[!is.na(client_lims)]
        }
      }
      
      usr <- usr_from_lim(lims, type=par()$yaxs) 
    } else {
      # is x
      if(length(grep("^x$", unlist(lapply(side_components, names)))) > 0){
        lims <- lims_from_list(lapply(side_components, var='x', function(list, var) strip_pts(list,var)))
        client_lims <- lims_from_client(side_components, var='xlim', side)
        lims[!is.na(client_lims)] <- client_lims[!is.na(client_lims)]
      } else {
        dataViews <- views[!(names(views) %in% "legend")]
        dataViews <- do.call(c, unname(dataViews))
        dataViews <- dataViews[!names(dataViews) %in% c("axis","gs.config")]
        dataViews <- do.call(c, unname(dataViews))
        xData <- do.call(c, unname(dataViews[grep("^x$", names(dataViews))]))
        
        lims <- range(xData,na.rm=TRUE)
        client_lims <- dataViews[grep("^xlim$", names(dataViews))]
        if(length(client_lims) > 0){
          client_lims <- client_lims[[1]]
          lims[!is.na(client_lims)] <- client_lims[!is.na(client_lims)]
        }
      }
      
      usr <- usr_from_lim(lims, type=par()$xaxs)
    }
    sides[[side]] = list(usr=usr)
    
  }
  names(sides) <- unique_sides
  return(sides)
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
  if (var %in% names(list))
    list[[var]]
  else
    NA
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

lims_from_list <- function(list){
  c(min(sapply(list, min),na.rm=TRUE), max(sapply(list, max),na.rm=TRUE))
}

usr_from_lim <- function(lim, type = 'i', log=FALSE){
  
  if (log)
    stop('log = TRUE not currently supported')
  usr <- switch (type,
    i = lim,
    r = c(lim[1]-0.04*diff(lim), lim[2]+0.04*diff(lim))
  )
  if (diff(usr) == 0){
    usr <- c(usr[1]-0.5, usr[2]+0.5)
  }
  return(usr)
}
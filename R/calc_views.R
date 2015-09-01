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
  
  views <- set_view_order(views, config("orderToPlot")$order)
  
  views <- set_window(views)
  
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
      if(length(draw_sides) == 1){
        view_i <- which(sapply(unique_sides, function(x) x[1] == draw_sides))
        to_draw <- setNames(list(gsplot[[i]][['arguments']]), names(gsplot[i]))
        
        if(draw_sides %% 2 == 0){
          draw_sides <- c(1,draw_sides)
        } else {
          draw_sides <- c(draw_sides,2)
        }
        
        views[[view_i]] <- list(window=list(side=draw_sides))
        views[[view_i]] <- append(views[[view_i]], to_draw)           
      } else {
        # // to do: verify sides are in order: x then y
        view_i <- which(sapply(unique_sides, function(x) x[1] == draw_sides[1] & x[2] == draw_sides[2]))
        to_draw <- setNames(list(gsplot[[i]][['arguments']]), names(gsplot[i]))
        views[[view_i]] <- append(views[[view_i]], to_draw)        
      }
      

    } else {
      # // if field isn't associated with a side(s), it is moved up to top level (e.g., legend)
      newList <- list()
      var <- names(gsplot[i])
      newList[[var]] <- gsplot[[i]]
      views <- append(views, newList)
    }
  }
  
  return(views)
}

set_view_list <- function(views, var, na.action=NA, remove=TRUE){
  view_i <- which(names(views) %in% "view")
  for (i in view_i){
    values <- lapply(views[[i]], function(x) strip_pts(x, var))
    val.i <- which(!is.na(values)) # which row to use
    if (length(val.i) == 0){
      values = na.action
    } else {
      if (length(unique(rownames(values[val.i]))) > 1)
        warning('for view ', i,', more than one ',var,' specified. ', unique(rownames(values[val.i])))
      values <- unname(values[val.i])[[1]]
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
  views <- set_view_list(views, var = 'ylab', na.action="")
  set_view_list(views, var = 'xlab', na.action="")
}


set_view_lim <- function(views){
  views <- set_view_list(views, var = 'xlim', na.action=NA)
  views <- set_view_list(views, var = 'ylim', na.action=NA)
  

  data <- list(y=summarize_args(views,c('y','y1','y0'),ignore=c('window','gs.config')), 
               x=summarize_args(views,c('x','x1','x0'),ignore=c('window','gs.config')))
  axs <- list(yaxs=summarize_args(views,c('yaxs'),ignore=c('window','gs.config')),
              xaxs=summarize_args(views,c('xaxs'),ignore=c('window','gs.config')))
  
  definedSides <- unlist(c_unname(views),recursive = FALSE)
  definedSides <- unique(unname(unlist(definedSides[grep("side", names(definedSides))])))

  for(var in c('y','x')){
    for (i in names(data[[var]])){
      n.i <- as.numeric(i)
      lim.name <- paste0(var,'lim')
      axs.name <- paste0(var, 'axs')
      view.side <- get_view_side(views, as.numeric(i), var)
      match.side <- as.character(views_with_side(views, view.side))
      data.var <- c_unname(data[[var]][match.side])
      
      if(all(is.na(data.var))){
        if((view.side %% 2) == 0){ #even
          otherSide <- c(2,4)[c(2,4) %in% definedSides[definedSides != view.side]]
        } else { #odd
          otherSide <- c(1,3)[c(1,3) %in% definedSides[definedSides != view.side]]
        }
        data.var <- c_unname(data[[var]][otherSide])
        if(all(is.na(data.var))){ #So do data total
          data.var <- c(0,1)
        }
      }
      
      data.lim <- range(data.var[is.finite(data.var)])
      usr.lim <- views[[n.i]][['window']][[lim.name]][1:2]
      views[[n.i]][['window']][[lim.name]] <- data.lim
      views[[n.i]][['window']][[lim.name]][!is.na(usr.lim)] <- usr.lim[!is.na(usr.lim)]
      
      usr.axs <- axs[[axs.name]][[n.i]]
      
      if (any(!is.na(usr.axs)) && usr.axs[which(!is.na(usr.axs))] == 'o') {
        view.i <- which(!names(views[[n.i]]) %in% c('window', 'gsplot'))[which(!is.na(usr.axs))]
        buffer <- 0.04*views[[n.i]][['window']][[lim.name]][[2]] #4% buffer based on upper limit
        buffer.lim <- views[[n.i]][['window']][[lim.name]][[which(is.na(usr.lim))]]
        buffer.lim <- ifelse(which(is.na(usr.lim)) == 1, buffer.lim - buffer, buffer.lim + buffer)
        views[[n.i]][['window']][[lim.name]][[which(is.na(usr.lim))]] <- buffer.lim
        views[[n.i]][[view.i]][[axs.name]] <- NULL
        views[['par']][[axs.name]] <- 'i'
      }
  
    }

  }

  return(views)
}

c_unname <- function(list){
  unname(do.call(c, list))
}

unname_c <- function(list){
  do.call(c, unname(list))
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
    values[[i]] <- c_unname(valStuff)
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
    else {
      u.list <- unname_c(list)
      if(v %in% names(u.list))
        out <- append(out, u.list[[v]])
      else
        out <- append(out, NA)
    }
  }
  return(out)
}

set_window <- function(list){
  
  listOut <- list
  pars <- list[['par']]
  
  for(j in which(names(list) == "view")){
    
    window <- list[[j]][['window']]
    plots <- list[[j]]
    plots[['window']] <- NULL
    
    var <- c("axes","ann","frame.plot") #Add panel.first, panel.last, asp, and "main","sub","frame.plot"...without breaking title
    varPar <- c("xaxs","yaxs","xaxt","yaxt","las")
    
    window[var] <- TRUE
    
    for(i in names(plots)){
      for(k in var){
        if(k %in% names(plots[[i]])){
          window[[k]] <- plots[[i]][[k]]
        } 
        plots[[i]][[k]] <- NULL
      }
      pars[varPar[varPar %in% names(plots[[i]])]] <- plots[[i]][names(plots[[i]]) %in% varPar]
      plots[[i]][names(plots[[i]]) %in% varPar] <- NULL
    }
    
    for (h in which(names(list) == "axis")) {
      if(list[[h]][['arguments']][['side']] %in% window[['side']]) {
        reverse <- list[[h]][['gs.config']][['reverse']]
        if (!is.null(reverse) && reverse ) {
          sideToReverse <- list[[h]][['arguments']][['side']]
          axes <- ifelse(sideToReverse %% 2 == 0, 'y', 'x')
          axisReverse <- paste0(axes, "lim")
          window[[axisReverse]] <- rev(window[[axisReverse]])
        }
      }
    }
    
    listOut[[j]] <- plots
    listOut[[j]][['window']] <- window
  }
  
  listOut[['par']] <- pars
  
  return(listOut)
}

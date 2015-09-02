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

views <- function(gsplot){
  gsplot[names(gsplot) %in% 'view']
}

group_views <- function(gsplot){
  tail.gs <- gsplot[[length(gsplot)]]
  tail.nm <- names(gsplot[length(gsplot)])
  gsplot[[length(gsplot)]] <- NULL
  views <- gsplot # existing
  add_sides <- set_sides(tail.gs[['gs.config']][['side']])
    
  if (!is.null(add_sides)){
    to_draw <- setNames(list(tail.gs[['arguments']]), tail.nm)
    # // to do: verify sides are in order: x then y
    view.1 <- views_with_side(views, add_sides[1])
    view.2 <- views_with_side(views, add_sides[2])
    if (!is.null(view.1) && !is.null(view.2) && any(view.2==view.1)){
      v.i = view.2[which(view.2==view.1)]
      views[[v.i]] <- append(views[[v.i]], to_draw)
    } else{
      views <- append(list(view = append(list(window=list(side=add_sides)), to_draw)), views)
    }
  } else {
    # // if field isn't associated with a side(s), it is moved up to top level (e.g., legend)
    newList <- list()
    var <- tail.nm
    newList[[var]] <- tail.gs
    views <- append(views, newList)
  }

  return(views)
}

set_sides <- function(sides){
  if (length(sides)==1){
    ifelse(sides %% 2 == 0, c(1,sides), c(sides,2))
  } 
  return(sides)
}
set_view_list <- function(views, var, na.action=NA, remove=TRUE, ignore=NULL){
  view_i <- which(names(views) %in% "view")
  for (i in view_i){
    values <- lapply(views[[i]][!names(views[[i]]) %in% ignore], function(x) strip_pts(x, var))
    val.i <- unname(which(!is.na(values))) # which row to use
    if (length(val.i) == 0){
      values = na.action
    } else {
      values <- unname_c(values[val.i])
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
  views <- set_view_list(views, var = 'xlim', na.action=NA, ignore='window')
  views <- set_view_list(views, var = 'ylim', na.action=NA, ignore='window')
  

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
        buffer <- lim_buffer(views[[n.i]][['window']], lim.name)
        lim <- views[[n.i]][['window']][[lim.name]][[which(is.na(usr.lim))]]
        buffered.lim <- ifelse(which(is.na(usr.lim)) == 1, lim - buffer, lim + buffer)
        views[[n.i]][['window']][[lim.name]][[which(is.na(usr.lim))]] <- buffered.lim
        views[[n.i]][[view.i]][[axs.name]] <- NULL
        views[['par']][[axs.name]] <- 'i'
      }
  
    }

  }

  return(views)
}

lim_buffer <- function(window, lim.name, buffer=0.04){
  # needs to read window[['log']] and use a different action if grepl(substr(lim.name,1,1), window[['log']])
  if (grepl(substr(lim.name,1,1), window[['log']]))
    stop('logged buffer not implemented')
  
  return(0.04*diff(window[[lim.name]]))
  
}
c_unname <- function(list){
  unname(do.call(c, list))
}

unname_c <- function(list){
  do.call(c, unname(list))
}
views_with_side <- function(views, side){
  with.side = lapply(views, function(x) any(x[['window']][['side']] %in% side))
  view.match = unname(unlist(with.side[names(with.side) == 'view']))
  if (is.null(view.match) || !any(view.match))
    return(NULL)
  else
    return(which(view.match))
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

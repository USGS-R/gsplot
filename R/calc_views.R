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

  views <- set_view_lab(views)
  
  #views <- set_view_order(views, config("orderToPlot")$order)
  
  views <- set_window(views)
  
  return(views)
}

#' get the indices of the gsplot object for where the views are
#' 
#' @param gsplot a gsplot object
#' @return a vector of ints
which_views <- function(gsplot){
  grep('view.', names(gsplot))
}

which_sides <- function(gsplot){
  grep('side.', names(gsplot))
}
view_names <- function(gsplot){
  names(views(gsplot))
}

views <- function(gsplot){
  gsplot[which_views(gsplot)]
}

sides <- function(gsplot){
  gsplot[which_sides(gsplot)]
}
non_views <- function(gsplot, include.sides = TRUE){
  non.views <- gsplot
  non.views[which_views(non.views)] <- NULL
  if (!include.sides)
    non.views[which_sides(non.views)] <- NULL
  return(non.views)
}

append_sides <- function(gsplot, sides, on.exists = c('skip','replace')){
  
  if (is.null(sides))
    return(gsplot)
  on.exists = match.arg(on.exists)
  
  side_template <- list(lim = c(NA, NA), label=NA, usr.lim=c(FALSE, FALSE))
  
  if (on.exists == 'skip'){
    sides <- as.side_name(sides)
    
    to_add <- !sides %in% names(gsplot)
    side_list <- rep(list(side_template), sum(to_add)) %>% setNames(sides[which(to_add)])
    gsplot <- append(gsplot, side_list)
  } else if (on.exists == 'replace'){
    stop('on.exists ', on.exists, ' not implemented yet')
  }
  return(gsplot)
}
                       

group_views <- function(gsplot){
  tail.gs <- gsplot[[length(gsplot)]]
  tail.nm <- names(gsplot[length(gsplot)])
  gsplot[[length(gsplot)]] <- NULL
  add_sides <- set_sides(tail.gs[['gs.config']][['side']])
  non.views <- non_views(gsplot, include.sides = FALSE)
  vew.n.sde <- gsplot[c(which_views(gsplot), which_sides(gsplot))]
  if (!is.null(add_sides)){
    vew.n.sde <- append_sides(vew.n.sde, add_sides)
    to_draw <- setNames(list(c(tail.gs[['arguments']], legend.name=tail.gs[['gs.config']][['legend.name']])), tail.nm)
    view.name <- sprintf('view.%s.%s',add_sides[1],add_sides[2])
    sides <- sides(vew.n.sde)
    sides <- set_side_lim(list(to_draw) %>% setNames(view.name), sides)
    
    vew.n.sde <- append_replace(vew.n.sde, sides)
    
    # // to do: verify sides are in order: x then y
    
    if (!is.null(vew.n.sde[[view.name]])){
      vew.n.sde[[view.name]] <- append(vew.n.sde[[view.name]], to_draw)
      vew.n.sde[[view.name]][['window']][['par']] <- append_replace(gsplot[[view.name]][['window']][['par']], tail.gs[['gs.config']][['par']])
    } else{
      new.view <- list(append(to_draw, list(window=list(side=add_sides,par=tail.gs[['gs.config']][['par']])))) %>% 
        setNames(view.name)
      vew.n.sde <- append(vew.n.sde, new.view)
    }
  } else {
    # // if field isn't associated with a side(s), it is moved up to top level (e.g., legend)
    newList <- list()
    newList[[tail.nm]] <- tail.gs
    non.views <- append(non.views, newList)
  }

  return(append(vew.n.sde, non.views))
}

append_replace <- function(old.list, new.list){
  out.list <- old.list
  out.list[names(old.list) %in% names(new.list)] <- new.list[names(out.list[names(old.list) %in% names(new.list)])]
  new.list[names(new.list) %in% names(old.list)] <- NULL
  out.list <- append(out.list, new.list)
  return(out.list)
}
set_sides <- function(sides){
  if (length(sides)==1){
    if(sides %% 2 == 0)
      sides = c(1,sides)
    else 
      sides = c(sides,2)
  } 
  return(sides)
}

which_reals <- function(values, na.value){
  
  if (is.na(na.value))
    return(which(!is.na(values)))
  else
    return(which(!is.na(values) & values != na.value)) # which row to use. goofy because values != NA is always NA, not logical
  
}
set_view_window <- function(views, param, na.value=NA, remove=TRUE, ignore=NULL){
  view_i <- which_views(views)
  for (i in view_i){
    values <- lapply(views[[i]][!names(views[[i]]) %in% ignore], function(x) strip_pts(x, param))
    val.i <- which_reals(values, na.value)
    if (length(val.i) == 0){
      values = na.value
    } else {
      values <- unname_c(values[val.i])
    }
    if (remove)
      views[[i]] <- lapply(views[[i]], function(x) remove_field(x, param))
    
    views[[i]][['window']][[param]] <- values
  }
  
  return(views)
}

as.view_name <- function(sides){
  paste0('view.', paste(sides, collapse='.'))
}


as.side_name <- function(sides){
  paste('side.',sides, sep='')
}

as.side <- function(side.names){
  as.numeric(gsub('side.','',side.names))
}
set_view_log <- function(views){
  set_view_window(views, param = 'log', na.value="")
}

set_view_lab <- function(views){
  views <- set_view_window(views, param = 'ylab', na.value="")
  set_view_window(views, param = 'xlab', na.value="")
}

set_usr_lim <- function(lims, sides){
  for (lim in names(lims)){
    to.set <- !is.na(lims[[lim]])
    sides[[lim]]$lim[to.set] <- lims[[lim]][to.set]
    sides[[lim]]$usr.lim[to.set] <- TRUE
  }
  return(sides)
}

locked_sides <- function(sides){
  lim.locks <- sapply(sides, function(x) all(x$usr.lim))
  names(lim.locks)[lim.locks]
}

set_side_lim <- function(view, sides){
  y.include <- c('y','y1','y0','ytop','ybottom')
  x.include <- c('x','x1','x0','xleft','xright')
  # // need value arguments, need yaxs/xaxs args, need user-specified ylim/xlim values
  usr.lims <- c(summarize_side_values(view, 'ylim', ignore=c('window','gs.config'), axis='y'),
                summarize_side_values(view, 'xlim', ignore=c('window','gs.config'), axis='x'))
  sides <- set_usr_lim(usr.lims, sides)
  
  locked.sides <- locked_sides(sides)
  side.vals <- c(summarize_side_values(view, y.include, ignore=c('window','gs.config'), axis='y', skip.side = locked.sides),
                 summarize_side_values(view, x.include, ignore=c('window','gs.config'), axis='x', skip.side = locked.sides))

  for (side in names(side.vals)){
    data.vals <- side.vals[[side]]
    if (any(!is.na(data.vals))){
      data.range <- range(c(data.vals[is.finite(data.vals)], sides[[side]]$lim), na.rm = TRUE)
      free.lim <- !sides[[side]]$usr.lim
      data.range[!free.lim] <- sides[[side]]$lim[!free.lim]
      sides[[side]]$lim <- data.range
    }
  }
  #axs <- list(yaxs=summarize_args(views, c('yaxs'), ignore=c('gs.config')),
  #            xaxs=summarize_args(views, c('xaxs'), ignore=c('gs.config')))
  
      # usr.axs <- axs[[axs.name]][[n.i]]
      # 
      # if (any(!is.na(usr.axs)) && any(usr.axs == 'o')) {
      #   if (all(!is.na(usr.lim)))
      #     stop('no NA given to distinguish buffered limit')
      #   
      #   view.i <- which(!names(views[[n.i]]) %in% c('window', 'gs.config'))
      #   buffer <- 0.04*diff(views[[n.i]][['window']][[lim.name]])
      #   lim <- views[[n.i]][['window']][[lim.name]][[which(is.na(usr.lim))]]
      #   buffered.lim <- ifelse(which(is.na(usr.lim)) == 1, lim - buffer, lim + buffer)
      #   views[[n.i]][[view.i]][[lim.name]][[which(is.na(usr.lim))]] <- buffered.lim
      #   views[[n.i]][['window']][[lim.name]][[which(is.na(usr.lim))]] <- buffered.lim
      #   views[[n.i]][['window']][['par']][[axs.name]] <- NULL
      #   views[['par']][[axs.name]] <- 'i'
      # }
  
    #}

#  }

  return(sides)
}


c_unname <- function(list){
  unname(do.call(c, list))
}

unname_c <- function(list){
  do.call(c, unname(list))
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

summarize_args <- function(views, param, na.value, ignore='gs.config'){
  
  view_i <- which_views(views)
  values <- list()
  for (i in view_i){
    x <- views[[i]][!names(views[[i]]) %in% ignore]
    valStuff <- lapply(x, function(x) strip_pts(x, param))
    values[[i]] <- c_unname(valStuff)
  }
  names(values) <- view_i
  return(values)
}

summarize_side_values <- function(view, param, na.value=NULL, axis=c('x','y'), ignore='gs.config', skip.side=NA){
  axis <- match.arg(axis)
  side_i <- c('x'=1,'y'=2)
  
  view_nm <- names(view)[which_views(view)] #// is it a view? if not, pass through
  if (length(view_nm) == 0){
    return(na.value)
  }
  
  side <- as.side_name(strsplit(view_nm, '[.]')[[1]][1 + side_i[[axis]]])
  
  if (side %in% skip.side)
    return(na.value)
  
  x <- view[!names(view) %in% ignore]
  valStuff <- lapply(x, function(x) strip_pts(x[[1]], param))
  if (length(valStuff) == 1 & is.na(valStuff))
    return(na.value)
  values <- list(c_unname(valStuff)) %>% 
    setNames(side)
  return(values)
}

remove_field <- function(list, param){
 
  for (v in param){
    if (v %in% names(list))
      list[[v]] <- NULL
  }
  return(list)
}

strip_pts <- function(list, param){
  out <- c()
  for (v in param){
    if (v %in% names(list) &&  !inherits(list[[v]], c('function','formula')))
      out <- append(out, list[[v]])
    else{
      if (any(sapply(list, is.list))){
        u.list <- unname_c(list[sapply(list, is.list)])
        if(v %in% names(u.list))
          out <- append(out, u.list[[v]])
        else if (any(sapply(u.list, function(x) any(names(x) %in% v))))
          out <- append(out, u.list[[which(sapply(u.list, function(x) any(names(x) %in% v)))]][[v]])
        else
          out <- append(out, NA)
      } else
        out <- append(out, NA)
      
    }
    
  }
  return(out)
}

set_window <- function(list){
  
  listOut <- list
  pars <- list[['par']]
  
  for(j in which_views(list)){
    
    window <- list[[j]][['window']]
    plots <- list[[j]]
    plots[['window']] <- NULL
    
    param <- c("axes","ann","frame.plot") #Add panel.first, panel.last, asp, and "main","sub","frame.plot"...without breaking title
    varPar <- c("xaxs","yaxs","xaxt","yaxt","las")
    
    window[param[!(param %in% names(window))]] <- TRUE
    
    for(i in names(plots)){
      for(k in param){
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

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


set_view_log <- function(views){
  set_view_window(views, param = 'log', na.value="")
}

set_view_lab <- function(views){
  views <- set_view_window(views, param = 'ylab', na.value="")
  set_view_window(views, param = 'xlab', na.value="")
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

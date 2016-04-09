#' gsplot show
#'
#' Shows gsplot in the plot window. 
#'
#' @param x gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments.
#' 
#' @importFrom graphics mtext
#' @importFrom graphics plot.new
#' @importFrom graphics box
#' @importFrom graphics plot.xy
#' @export
#' @examples
#' gs <- gsplot() %>%
#'    points(1, 2, legend.name="Cool points", xlim=c(0,NA)) %>%
#'    lines(x=1:5, y=1:5, legend.name="Cool lines", ylab='taco night') %>%
#'    legend(location="topleft")
#' print(gs)
#' 
#' # dropping 'print()' around the object works the same way 
#' # (however, use 'print()' explicitly when in a loop)
#' gs <- gsplot() %>%
#'    points(1, 2, legend.name="Cool points", xlim=c(0,NA)) %>%
#'    lines(x=1:5, y=1:5, legend.name="Cool lines", ylab='taco night') %>%
#'    legend(location="topleft")
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1, 2) %>%
#'    lines(x=1:5, y=1:5, side=c(3,4)) 
#' gs
print.gsplot <- function(x, ...){
  
  # -- set plot -- 
  views = x
  
  if(!isTRUE(x[['par']][['new']])){
    dev.hold()
    on.exit(dev.flush())
    plot.new()
  }

  i.axis <- which(names(views) %in% 'axis')
  defined.sides <- sapply(i.axis, function(x) views[[x]][['arguments']][['side']])
  
  bg.arg <- views$bgCol
  title.arg <- views$title
  
  view.info <- view_info(views)
  view.index <- view.info$index
  view.names = view_names(views)
  for (view.name in view.names){

    plots = views[[view.name]]
    plots[['window']] <- NULL
    window = views[[view.name]][['window']]
    
    par(views[['par']])
    
    par(window[['par']])
    x.side = as.side_name(window$side[1])
    y.side = as.side_name(window$side[2])
    
    plot.window(xlim = views[[x.side]]$lim, ylim = views[[y.side]]$lim, log = view.info$log[view.name==view.names])

    # -- initial view --
    if(view.name == view.names[1]){
      if (!is.null(bg.arg))
        bgCol(bg.arg)
      title(title.arg)
    }
    
    # // *** this is a carryover from the indexing. Should alter view.info to use names ***
    i = which(view.name==view.names) 
    # -- call functions -- 
    
#     if((sum(view.info$x.side.defined.by.user[i], view.info$y.side.defined.by.user[i])== 0 ) &
#        (class(window$xlim) == "numeric" & class(window$ylim) == "numeric") | 
    if(!(any(names(plots) %in% 'grid'))){
      to_gsplot(lapply(plots, function(x) x[!names(x) %in% 'legend.name']))
    } else {
      draw_custom_grid(views,i)
      plots <- plots[!(names(plots) %in% 'grid')]
      to_gsplot(lapply(plots, function(x) x[!(names(x) %in% c('legend.name'))]))
    }

    
    if(!view.info$x.side.defined.by.user[i]){
      if(window$axes){
        Axis(side=view.info$x[i],x=window$xlim)
      }
    } else {
      x.axis <- i.axis[which(defined.sides == view.info$x[i])]
      draw_axis(views, index.axis=x.axis)
    }
    
    if(!view.info$y.side.defined.by.user[i]){
      if(window$axes){
        Axis(side=view.info$y[i],x=window$ylim)
      } 
    } else {
      y.axis <- i.axis[which(defined.sides == view.info$y[i])]
      draw_axis(views, index.axis=y.axis)
    }
    
    if(window$ann){
      mtext(text=window$xlab, side=window$side[1], line = 2, las=config("mtext")$las)
      mtext(text=window$ylab, side=window$side[2], line = 2, las=config("mtext")$las)        
    }
    
    par(new=TRUE)
  }
  
  i.axis.noview <- i.axis[which(!defined.sides %in% c(view.info$x, view.info$y))]
  draw_axis(views, index.axis=i.axis.noview)
  
  if(window$frame.plot){
    box()
  }

  draw_legend(views)
  
}

to_gsplot <- function(x){
  for (i in seq_len(length(x))){
    do.call(names(x[i]),x[[i]])
  }
}

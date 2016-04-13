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
  side.names <- side_names(views)
  
  for (side.name in side.names){
    side <- as.side(side.name)
    if(!view.info$x.side.defined.by.user[view.info$x==side]){ ## need to make it side specific (not x vs y)
      if(window$axes){
        Axis(side=side,x=lim(views, side))
      }
    } else {
      x.axis <- i.axis[which(defined.sides == view.info$x[i])]
      draw_axis(views, index.axis=x.axis)
    }
    
    if(!view.info$y.side.defined.by.user[i]){
      if(window$axes){
        Axis(side=view.info$y[i],x=ylim(views, y.side))
      } 
    } else {
      y.axis <- i.axis[which(defined.sides == view.info$y[i])]
      draw_axis(views, index.axis=y.axis)
    }
    
    if(window$ann){
      mtext(text=xlab(views, x.side), side=x.side, line = 2, las=config("mtext")$las)
      mtext(text=ylab(views, y.side), side=y.side, line = 2, las=config("mtext")$las)        
    }
  }
  
  view.info <- view_info(views)
  view.names = view_names(views)
  for (view.name in view.names){

    plots = views[[view.name]]
    window = plots[['window']]
    plots[['window']] <- NULL
    
    x.side <- as.x_side(view.name)
    y.side <- as.y_side(view.name)
    
    par(views[['par']])
    par(window[['par']])

    plot.window(xlim = xlim(views, x.side), ylim = ylim(views, y.side), log = as.log(views, view.name))

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

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

  i <- which(names(views) %in% 'axis')
  definded.sides <- sapply(i, function(x) views[[x]][['arguments']][['side']])
  
  bg.arg <- views$bgCol
  title.arg <- views$title
  
  view.info <- view_info(views)
  view.index <- view.info$index
  
  for (i in view.index){

    plots = views[[i]]
    plots[['window']] <- NULL
    window = views[[i]][['window']]
    
    par(views[['par']])
    
    par(window[['par']])
    plot.window(xlim = window$xlim, ylim = window$ylim, log = view.info$log[i==view.info$index])

    # -- initial view --
    if(i == view.index[1]){
      bgCol(bg.arg)
      title(title.arg)
    }
    
    # -- call functions -- 
    
    if((sum(view.info$x.side.defined.by.user[i], view.info$y.side.defined.by.user[i])== 0 ) &
       (class(window$xlim) == "numeric" & class(window$ylim) == "numeric") | 
       !(any(names(plots) %in% 'grid'))){
      to_gsplot(lapply(plots, function(x) x[!names(x) %in% 'legend.name']))
    } else {
      draw_custom_grid(views,i)
      plots <- plots[!(names(plots) %in% 'grid')]
      to_gsplot(lapply(plots, function(x) x[!(names(x) %in% c('legend.name'))]))
    }

    if(window$axes){
      if(!view.info$x.side.defined.by.user[i]){
        Axis(side=view.info$x[i],x=window$xlim)
      }
      if(!view.info$y.side.defined.by.user[i]){
        Axis(side=view.info$y[i],x=window$ylim)
      }
    } 
    
    if(window$ann){
      mtext(text=window$xlab, side=window$side[1], line = 2, las=config("mtext")$las)
      mtext(text=window$ylab, side=window$side[2], line = 2, las=config("mtext")$las)        
    }
    
    par(new=TRUE)
  }
  
  draw_axis(views)
  
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

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
print.gsplot <- function(x, ...){
  
  # -- set plot -- 
  views = calc_views(x)
  
  if(!isTRUE(x[['par']][['new']])){
    dev.hold()
    on.exit(dev.flush())
    plot.new()
  }

  
  for (i in which(names(views) %in% 'view')){
    plots = views[[i]]
    plots[['window']] <- NULL
    window = views[[i]][['window']]
    
    par(views[['par']])
    
    plot.window(xlim = window$xlim, ylim = window$ylim, log = window$log)

    # -- call functions -- 
    to_gsplot(plots)

    if(!("axis" %in% names(plots))){ 
      Axis(side=window$side[1],x=window$xlim)
      Axis(side=window$side[2],x=window$ylim)
    }

    mtext(text=window$xlab, side=window$side[1], line = 2)
    mtext(text=window$ylab, side=window$side[2], line = 2)
    
    par(new=TRUE)
  }
  box()
  
  draw_legend(x)

  # par(defaultPar)
  
}

to_gsplot <- function(x){
  for (i in seq_len(length(x))){
    do.call(names(x[i]),x[[i]])
  }
}

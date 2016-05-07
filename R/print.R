#' gsplot show
#'
#' Shows gsplot in the plot window. 
#'
#' @param x gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments.
#' 
#' @importFrom graphics mtext plot.new box plot.xy
#' @importFrom graphics Axis axTicks plot.window
#' @importFrom grDevices dev.flush dev.hold
#' @importFrom stats median na.omit
#' @importFrom utils find getS3method packageName tail
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

  if(!isTRUE(x$gobal$par$new)){
    dev.hold()
    on.exit(dev.flush())
    plot.new()
  }
  
  par(x$global$par)
  i.axis <- which(names(views) %in% 'axis')
  defined.sides <- sapply(i.axis, function(x) views[[x]][['arguments']][['side']])
  
  bg.arg <- views$bgCol
  title.arg <- views$title
  
  view.info <- view_info(views)
  side.names <- side_names(views)

  for (side.name in side.names){
    side <- as.side(side.name)
    par(x[[side.name]]$par)
    set_frame(views, side)
    if(!side %in% defined.sides){ 
      if(x[[side.name]][['axes']]){
        Axis(side=side,x=lim(views, side))
      }
    } else {
      axis <- i.axis[which(defined.sides == side)]
      draw_axis(views, index.axis=axis)
    }
    if(exists('ann', x[[side.name]]$par) && x[[side.name]]$par$ann){
      mtext(text=label(views, side), side=side, line = 2, las=config("mtext")$las)
    }
  }
  
  for (view.name in view_names(views)){
    par(x$global$par)
    x.side.name <- as.x_side_name(view.name)
    y.side.name <- as.y_side_name(view.name)
    par(x[[x.side.name]]$par)
    par(x[[y.side.name]]$par)
    set_frame(views, side=view.name)
    if(any(names(views[[view.name]]) %in% 'grid')){
      draw_custom_grid(views,view.name)
    }
    print.view(views[[view.name]])
        # -- initial view --
    if(view.name == view_names(views)[1]){
      if (!is.null(bg.arg))
        bgCol(bg.arg)
      title(title.arg)
    }
    par(new=TRUE)
  }
  
  i.axis.noview <- i.axis[which(!defined.sides %in% c(view.info$x, view.info$y))]
  draw_axis(views, index.axis=i.axis.noview)

  draw_legend(views)
  par(new=FALSE)
}

#' print view
#' 
#' @param x a view
#' @param \dots additional arguments (not used)
#' @keywords internal
print.view <- function(x, ...){
  plots <- remove_field(x, param = c('par','grid','window'))
  
  # if(window$frame.plot){
  #   box()
  # } 
  par(x[['par']])
  
  # -- call functions -- 
  to_gsplot(lapply(plots, function(x) x[!(names(x) %in% c('legend.name'))]))
}

to_gsplot <- function(x){
  for (i in seq_len(length(x))){
    do.call(names(x[i]),x[[i]])
  }
}

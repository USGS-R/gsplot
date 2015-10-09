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
  
  view.info <- view_info(views)
  view.sides.drawn <- NULL
  
  for (i in which(names(views) %in% 'view')){

    plots = views[[i]]
    plots[['window']] <- NULL
    window = views[[i]][['window']]
    
    par(views[['par']])
    
    par(window[['par']])
    plot.window(xlim = window$xlim, ylim = window$ylim, log = view.info$log[i==view.info$index])

    sides.not.defined <- window$side[!(window$side %in% definded.sides)]
    
    if(!is.null(view.sides.drawn)){
      view.sides.drawn <- sides.not.defined[-view.sides.drawn]
    }
    
    if(window$axes){
      for(j in sides.not.defined){
        if(j %% 2 != 0){
          Axis(side=j,x=window$xlim)
        } else {
          Axis(side=j,x=window$ylim) 
        }
        view.sides.drawn <- append(view.sides.drawn, j)
      }
    } 
    
    if(window$ann){
      mtext(text=window$xlab, side=window$side[1], line = 2)
      mtext(text=window$ylab, side=window$side[2], line = 2)        
    }
    
    # -- call functions -- 
    to_gsplot(lapply(plots, function(x) x[!names(x) %in% 'legend.name']))

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

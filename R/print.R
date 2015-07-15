#' gsplot show
#'
#' show gsplot
#'
#' @param x gsplot object
#' @param \dots stuff
#' @return modified gsplot object 
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
#' gs
print.gsplot <- function(x, ...){

  gsOptions <- do.call(c, unname(options("gsplot")))

  if(!("new" %in% names(gsOptions))){
    dev.hold()
    on.exit(dev.flush())
    plot.new()
  }
  
  # -- set plot -- 
  views = calc_views(x)
  
  defaultPar <- par(no.readonly = TRUE)#, mar=legend_adjusted_margins(x))
  
  for (i in which(names(views) %in% 'view')){
    plots = views[[i]]
    plots[['window']] <- NULL
    window = views[[i]][['window']]
    par(config("par")) 
    
    plot.window(xlim = window$xlim, ylim = window$ylim, log = window$log)
    
    # -- call functions -- 
    to_gsplot(plots)

    if(!("axis" %in% names(views[[i]]))){ 
      axis(side=window$side[1], config("axis"))
      axis(side=window$side[2], config("axis"))
    }

    mtext(text=window$xlab, side=window$side[1], line = 2)
    mtext(text=window$ylab, side=window$side[2], line = 2)
    
    par(new=TRUE)
  }
  box()
  
  draw_legend(x)

  par(defaultPar)
  
}

to_gsplot <- function(x){
  for (i in seq_len(length(x))){
    do.call(names(x[i]),x[[i]])
  }
}

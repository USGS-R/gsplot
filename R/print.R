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
  
  if(!(dev.cur() == 1) && !("new" %in% names(gsOptions))){
    plot.new()
  }
  
  # -- set plot -- 
  views = calc_views(x)
  
  defaultPar <- par(no.readonly = TRUE)#, mar=legend_adjusted_margins(x))
  
  for (i in which(names(views) %in% 'view')){
    view = views[[i]]
    
    par(xlog=view$gs.config$xlog, ylog=view$gs.config$ylog)
    par(usr=view$gs.config$usr)
    
    # // hard-coding set of logged axis for example of log='x'
    par(xaxp=c(1,10,3))
    
    par(config("par")) 
    
    # -- call functions -- 
    to_gsplot(view, which(!names(view)  %in% 'gs.config'))

    if(!("axis" %in% names(view))){
      axis(side=view$gs.config$side[1], config("axis"))
      axis(side=view$gs.config$side[2], config("axis"))
    }

    mtext(text=view$gs.config$xlab, side=view$gs.config$side[1], line = 2)
    mtext(text=view$gs.config$ylab, side=view$gs.config$side[2], line = 2)
    
    par(new=TRUE)
  }
  box()
  
  draw_legend(x)

  par(defaultPar)
}

to_gsplot <- function(x, which_i){
  for (i in which_i){
    do.call(names(x[i]),x[[i]])
  }
}

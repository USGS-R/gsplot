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
#'    points(1, 2, legend.name="Cool points") %>%
#'    lines(x=1:5, y=1:5, legend.name="Cool lines", ylab='taco night') %>%
#'    legend(location="top")
#' gs
print.gsplot <- function(x, ...){

  plot.new()
  # -- set plot -- 
  views = calc_views(x)
  
  defaultPar <- par(no.readonly = TRUE)#, mar=legend_adjusted_margins(x))
  
  for (i in which(names(views) %in% 'view')){
    view = views[[i]]
    
    par(usr=view$gs.config$usr)
    
    par(config("par"))
    
    axis(side=view$gs.config$side[1], config("axis"))
    axis(side=view$gs.config$side[2], config("axis"))

    mtext(text=view$gs.config$xlab, view$gs.config$side[1], line = 2)
    mtext(text=view$gs.config$ylab, view$gs.config$side[2], line = 2)

    # -- call functions -- 
    to_gsplot(view, which(!names(view)  %in% 'gs.config'))


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

#' gsplot show
#'
#' show gsplot
#'
#' @param x gsplot object
#' @param \dots stuff
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list()) %>%
#'    points(x=1, y=2, legend.name="Cool points") %>%
#'    lines(x=1:5, y=1:5, legend.name="Cool lines") 
#'    #legend()
#' gs
print.gsplot <- function(x, ...){

  plot.new()
  # -- set plot -- 
  views = calc_views(x)
  
  defaultPar <- par(no.readonly = TRUE, mar=legend_adjusted_margins(x))
  
  for (i in 1:length(views)){
    view = views[[i]]
    
    par(usr=view$usr)
    par(options("gsplot")[[1]])
    
    axis(side=view$gs.config$side[1], config("axis"))
    axis(side=view$gs.config$side[2], config("axis"))
    
    # par(defaultPar)
    # -- call lines -- 
    to_gsplot(view, which(names(view)  %in% 'lines'))
    
    # -- call points -- 
    to_gsplot(view, which(names(view)  %in% 'points'))
    par(new=TRUE)
  }
  box()
  
  draw_legend(x)

  par(new=FALSE)
}

to_gsplot <- function(x, which_i){
  for (i in which_i){
    do.call(names(x[i]),x[[i]])
  }
}

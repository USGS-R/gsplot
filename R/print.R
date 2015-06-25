#' gsplot show
#'
#' show gsplot
#'
#' @param x gsplot object
#' @param \dots stuff
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list())
#' gsNew <- points(gs, x=1, y=2)
#' gsNew
print.gsplot <- function(x, ...){
  
  # -- set plot -- 
  # will call plot.new
  views = calc_views(x)
  
  for (i in 1:length(views)){
    view = views[[i]]
    
    plot(x=NA,xlim=view[['xlim']],ylim=view[['ylim']], ylab=NA, xlab=NA, axes = F)
    axis(side=view[['gs.config']][['side']][1])
    axis(side=view[['gs.config']][['side']][2])
    
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

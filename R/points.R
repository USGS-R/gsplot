#' gsplot points
#'
#' Point stuff
#'
#' @param \dots stuff
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list(figure="testFig"))
#' gsNew <- points(gs, x=1, y=2)
points <- function(...){
  dots <- list(...)
  
  if(length(dots) != 0 && class(dots[[1]]) == "gsplot" ){
    gsplot <- dots[[1]]
    gsplot$points <- dots[-1]
    return(gsplot)    
  } else {
    graphics::points(...)
  }

}
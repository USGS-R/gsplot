#' gsplot bgCol
#'
#' bgCol stuff
#' 
#' @details Add additional functionality to points.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @rdname bgCol
#' @export
#' @examples
#' gs <- gsplot() %>%
#'    points(y=c(3,1,2), x=4:6, xlim=c(0,NA),legend.name="Points") %>%
#'    lines( c(3,4,3), c(2,4,6), legend.name="Lines", side=c(3,4)) %>%
#'    legend("topleft") %>%
#'    bgCol("lightgrey") 
#' gs
bgCol <- function(object, ...) {
  overrideGraphics("bgCol", object, ...)
}


bgCol.gsplot <- function(object, col, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("bgCol")
  arguments <- list(...)
  arguments <- append(list(col=col),arguments)
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(bgCol = list(arguments = arguments, 
                                 gs.config=list(legend.name = legend.name, 
                                                side = side))))
  return(gsplot(object))
}

#' @export
#' @rdname bgCol
#' @param col color
bgCol.default <- function(col,...){
  
  rect(par("usr")[1],par("usr")[3],par("usr")[2],par("usr")[4],col = col,...)
  
}



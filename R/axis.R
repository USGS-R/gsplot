#' gsplot axis
#'
#' Formats axes for the plotting region.
#'
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs: 
#' \itemize{
#'  \item{\code{side}} {integer indicating the bottom(1), left(2), top(3), or right(4) side of the plot to draw the axis}
#'  \item{\code{labels}} {logical value indicating whether numerical values are shown at the tickmarks, or a character vector of labels for each tickmark}
#'  }
#'    
#' @rdname axis
#' @export
#' @examples
#' 
#' gs <- gsplot() %>%
#'    points(x=1:5, y=1:5, legend.name="Stuff") %>%
#'    lines(2:6, y=2:6, ylim=c(0,10)) %>%
#'    axis(side=c(3,4),labels=FALSE) %>%
#'    legend("topright")
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),
#'            col="blue", pch=18, legend.name="Points", xlab="Index") %>%
#'    axis(side=c(3,4), labels=FALSE)
#' gs
#' 
axis <- function(object, ...) {
  override("graphics", "axis", object, ...)
}

axis.gsplot <- function(object, ..., side=c(1,2)) {
  
  current_list <- config("axis")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])

  arguments1 <- append(arguments, list(side=side[1]))
  object <- append(object,  list(axis = list(arguments = arguments1,
                                             gs.config=list(side = side))))
  
  if(length(side) > 1){
    arguments2 <- append(arguments, list(side=side[2]))    
    object <- append(object,  list(axis = list(arguments = arguments2, 
                                               gs.config=list(side = side))))   
  }


  return(gsplot(object))

}
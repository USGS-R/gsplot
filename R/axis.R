#' gsplot axis
#'
#' Formats axes for the plotting region.  See \code{\link[graphics]{axis}} for more details.
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
#'    axis(side=4, labels=FALSE)
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:10, 1:10, xaxs="i", yaxs="i") %>%
#'    axis(side=1, at = seq(0,10,by=0.1),labels=FALSE, tcl=-0.2)
#' gs
#' 
#' gs <- gsplot()%>%
#'    points(1:100, rnorm(100,mean=10000, sd=1000), log="y") %>%
#'    axis(side=c(3,4), labels=FALSE)
#' gs
axis <- function(object, ...) {
  override("graphics", "axis", object, ...)
}

axis.gsplot <- function(object, ...) {
  
  current_list <- config("axis")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  sides <- arguments$side
  arguments[["side"]] <- NULL
  for(i in sides){
    arguments1 <- append(arguments, list(side=i))
    object <- append(object,  list(axis = list(arguments = arguments1,
                                               gs.config=list())))    
  }
  
  return(gsplot(object))
  
}

draw_axis <- function(gsplot) {
  
  for(index in which(names(gsplot) %in% "axis")){
    axisParams <- gsplot[[index]][['arguments']]
    axis(axisParams)
  }
}
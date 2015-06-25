#' gsplot legend
#'
#' If called with gsplot as first argument, will cycle through the gsplot settings 
#' (points, lines, etc) and will produce a legend data structure to be added to the 
#' plot when it is rendered.
#'
#' @param legend.location specifies where the legend is relative to the plot. 
#'  Can use standard location parameter ("bottomright", "bottom", "bottomleft", "left", 
#'  "topleft", "top", "topright", "right" and "center") with the additions "below", "above", 
#'  "out-left", "out-right".
#' @param \dots stuff
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list(figure="testFig"))
#' gs <- points(gs, x=1, y=2, legend.name="Example Points", pch=1, col="blue")
#' gs <- legend(gs, location="topleft")
legend <- function(object, location="topright", ...){
  if (!missing(object) && class(object) == "gsplot" ){
    object$legend <- list(location = location, ...)
    return(object)
  } else {
    if (missing(object)){
      graphics::legend(...)
    } else {
      graphics::legend(object, ...)
    }
  }
}
#' gsplot draw_legend
#'
#' Will cycle through  the gsplot, looking at the legend configuration (if one exists), and
#' create legend entries based on the points, lines, etc. contained in the gsplot.
#' 
#' @param gsplot the gsplot to render legend on

draw_legend <- function(gsplot) {
  location <- gsplot[['legend']]$location
  legend(x=gsplot$legend$location, y=NULL, "YAAAAAAAAY", pch = 1, col = c(1, 3), cex = 1.2)
}





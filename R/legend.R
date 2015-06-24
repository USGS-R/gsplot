#' gsplot legend
#'
#' If called, will cycle through the gsplot settings (points, lines, etc)
#' and will produce a legend data structure to be added to the plot when it
#' is rendered.
#'
#' @param legend.position specifies where the legend is relative to the plot. 
#'  Can use standard location parameter ("bottomright", "bottom", "bottomleft", "left", 
#'  "topleft", "top", "topright", "right" and "center") with the additions "below", "above", 
#'  "out-left", "out-right".
#' @param \dots stuff
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list(figure="testFig"))
#' gsNew <- points(gs, x=1, y=2, legend.name="Example Points", pch=1, col="blue")
#' gsNew <- legend(gs, legend.position="below")
legend <- function(object, location="topright", ...){
  
  if (!missing(object) && class(object) == "gsplot" ){
    gsplot <- object
    
    gsplot$legend$location = location
    
    #pull point entries which need to be placed in legend
    pointList < gsplot$points
    gsplot$legend$entries = data.frame(text = pointList$legend.name,
                                       symbol = pointList$pch, 
                                       color = pointList$col, 
                                       line = pointList$lty, 
                                       stringsAsFactors = FALSE)
    
    #add line entries?
    
    return(gsplot(object))
  } else {
    if (missing(object)){
      graphics::points(location=location, ...)
    } else {
      graphics::points(object, location=location, ...)
    }
  }
}



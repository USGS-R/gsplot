#' gsplot legend
#'
#' If called with gsplot as first argument, will set the internal gsplot configuration
#' for legends
#'
#' @param legend.location specifies where the legend is relative to the plot. 
#'  Can use standard location parameter ("bottomright", "bottom", "bottomleft", "left", 
#'  "topleft", "top", "topright", "right" and "center") with the additions "below", "above", 
#'  "out-left", "out-right".
#' @param \dots normal legend params should forward through
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot(list(figure="testFig"))
#' gs <- points(gs, x=1, y=2, legend.name="Example Points 1", pch=1, col="blue")
#' gs <- points(gs, x=3, y=4, legend.name="Example Points 2", pch=5, col="red")
#' gs <- lines(gs, x=c(3,4,3), y=c(2,4,6), legend.name="Example Line", lty=1, col="orange")
#' gs <- lines(gs, x=c(2,6,8), y=c(1,6,9), lty=1, col="yellow")
#' gs <- legend(gs)
legend <- function(object, ...){
  overrideGraphics("legend", object, ...)
}


legend.gsplot <- function(object, location="topright", ...) {
  object <- append(object, list(legend = list(x = location, ...)))
  return(gsplot(object))
}

#' gsplot draw_legend
#'
#' Will cycle through  the gsplot, looking at the legend configuration (if one exists), and
#' create legend entries based on the points, lines, etc. contained in the gsplot.
#' 
#' @param gsplot the gsplot to render legend

draw_legend <- function(gsplot) {
  legendParams <- gsplot[['legend']]
  if(!is.null(legend)) {
    smartLegend <- data.frame(text = character(), 
                              symbol = numeric(), 
                              color = character(), 
                              line = numeric(), 
                              stringsAsFactors = FALSE)
    addToLegend <- function(newText, newSymbol, newColor, newLine) { 
      if(is.null(newText)) {
        newText <- ""
      }
      smartLegend <<- rbind(primary_legend, data.frame(text = newText, 
                                                       symbol = newSymbol, 
                                                       color = newColor, 
                                                       line = newLine, 
                                                       stringsAsFactors = FALSE))
    }
    
    #get legend entries for points
    pts_i <- which(names(gsplot) %in% 'points')
    for (i in pts_i){
      pts <- gs[[i]]
      addToLegend(pts$legend.name, pts$pch, pts$col, NA)
    }
    
    #get legend entries for lines
    lines_i <- which(names(gsplot) %in% 'lines')
    for (i in lines_i){
      lines <- gs[[i]]
      addToLegend(lines$legend.name, NA, lines$col, lines$lty)
    }
    
    #only include pch if we have a non-NA entry for points
    if(length(pts_i) > 0) {
      legendParams <- append(legendParams, list(
        pch=smartLegend$symbol
      ))
    }
    
    #only include lty if we have a non-NA entry for lines
    if(length(lines_i) > 0) {
      legendParams <- append(legendParams, list(
        lty=smartLegend$line
      ))
    }
    
    legendParams <- append(legendParams, list(
      legend=smartLegend$text, 
      col=smartLegend$color
      ))
    
    legend(legendParams,
           #x = location,  do we want to support explicit x/y coords? 
           y = NULL) 
  }
}





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
#' bottom <- gsplot(list()) %>% 
#'  points(x=1, y=2, side=c(3,2), legend.name="Example Points 1", pch=1, col="blue") %>% 
#'  points(x=3, y=4, side=c(1,4), legend.name="Example Points 2", pch=5, col="red") %>% 
#'  legend(location="bottom")
#' bottom
#' 
#' topright <- gsplot(list()) %>% 
#'  lines(x=c(3,4,3), y=c(2,4,6), legend.name="Example Lines", lty=5, col="orange") %>% 
#'  points(x=1, y=2, side=c(3,2), legend.name="Example Points 1", pch=1, col="blue") %>% 
#'  points(x=3, y=4, side=c(1,4), legend.name="Example Points 2", pch=5, col="red") %>% 
#'  legend(location="topright")
#' topright
#' 
#' above <- gsplot(list()) %>% 
#'  points(x=1, y=2, side=c(3,2), legend.name="Example Points 1", pch=1, col="blue") %>% 
#'  points(x=3, y=4, side=c(1,4), legend.name="Example Points 2", pch=5, col="red") %>% 
#'  legend(location="above")
#' above
#' 
#' below <- gsplot(list()) %>% 
#'  points(x=1, y=2, side=c(3,2), legend.name="Example Points 1", pch=1, col="blue") %>% 
#'  points(x=3, y=4, side=c(1,4), legend.name="Example Points 2", pch=5, col="red") %>% 
#'  legend(location="below")
#' below
#' 
#' toright <- gsplot(list()) %>% 
#'  points(x=1, y=2, side=c(3,2), legend.name="Example Points 1", pch=1, col="blue") %>% 
#'  lines(x=c(3,4,3), y=c(2,4,6), legend.name="Example Lines", lty=5, col="orange") %>% 
#'  points(x=3, y=4, side=c(1,4), legend.name="Example Points 2", pch=5, col="red") %>% 
#'  legend(location="toright")
#' toright
#' 
#' toleft <- gsplot(list()) %>% 
#'  points(x=1, y=2, side=c(3,2), legend.name="Example Points 1", pch=1, col="blue") %>% 
#'  points(x=3, y=4, side=c(1,4), legend.name="Example Points 2", pch=5, col="red") %>% 
#'  legend(location="toleft")
#' toleft
legend <- function(object, ...){
  overrideGraphics("legend", object, ...)
}


legend.gsplot <- function(object, location="topright", legend_offset=0.3, ...) {
  #TODO support explicit x/y coords
  y <- NULL
  inset <- NULL
  x <- NULL
  inset <- c(NA, NA)
  bty <- "o"
  if(location == "below") {
    x = "bottom"
    inset <- c(0, -legend_offset)
    bty <- "n"
    object <- append(object, list(legend = list(x = x, y = y, inset=inset, bty=bty, ...)))
  } else if(location == "above") {
    x = "top"
    inset <- c(0, -legend_offset)
    bty <- "n"
    object <- append(object, list(legend = list(x = x, y = y, inset=inset, bty=bty, ...)))
  } else if(location == "toright") {
    x = "right"
    inset <- c(-legend_offset, 0)
    bty <- "n"
    object <- append(object, list(legend = list(x = x, y = y, inset=inset, bty=bty, ...)))
  } else if(location == "toleft") {
    x = "left"
    inset <- c(-legend_offset, 0)
    bty <- "n"
    object <- append(object, list(legend = list(x = x, y = y, inset=inset, bty=bty, ...)))
  } else {
    x <- location
    object <- append(object, list(legend = list(x = x, ...)))
  }
  
  return(gsplot(object))
}

#' gsplot draw_legend
#'
#' Will cycle through  the gsplot, looking at the legend configuration (if one exists), and
#' create legend entries based on the points, lines, etc. contained in the gsplot.
#' 
#' @param gsplot the gsplot to render legend

draw_legend <- function(gsplot) {
  par(xpd=TRUE)
  legendParams <- gsplot[['legend']]
  if(!is.null(legendParams)) {
    smartLegend <- data.frame(text = character(), 
                              symbol = numeric(), 
                              color = character(), 
                              line = numeric(), 
                              stringsAsFactors = FALSE)
    getLegendItem <- function(newText, newSymbol, newColor, newLine) { 
      if(is.null(newText)) {
        newText <- ""
      }
      return(data.frame(text = newText, 
                                                       symbol = newSymbol, 
                                                       color = newColor, 
                                                       line = newLine, 
                                                       stringsAsFactors = FALSE))
    }
    
    #get legend entries for points
    pts_i <- which(names(gsplot) %in% 'points')
    for (i in pts_i){
      pts <- gsplot[[i]]
      smartLegend <- rbind(smartLegend, getLegendItem(pts[['gs.config']]$legend.name, pts[['arguments']]$pch, pts[['arguments']]$col, NA))
    }
    
    #get legend entries for lines
    lines_i <- which(names(gsplot) %in% 'lines')
    for (i in lines_i){
      lines <- gsplot[[i]]
      smartLegend <- rbind(smartLegend, getLegendItem(lines[['gs.config']]$legend.name, NA, lines[['arguments']]$col, lines[['arguments']]$lty))
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
    
    legend(legendParams) 
  }
}

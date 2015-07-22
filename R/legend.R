#' gsplot legend
#'
#' If called with gsplot as first argument, will set the internal gsplot configuration
#' for legends
#'
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs:
#' \itemize{
#'  \item{\code{location}} {position of the legend, specified by x- and y-coordinates or by keyword ("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", or "center")}
#'  \item{\code{title}} {character string indicating the legend title}
#' }
#'  
#' @export
#' @importFrom graphics par
#' @examples
#' 
#' topright <- gsplot() %>% 
#'  lines(x=c(3,4,3), y=c(2,4,6), legend.name="Lines", lty=5, col="orange") %>% 
#'  points(x=1, y=2, side=c(3,2), legend.name="Points 1", pch=1, col="blue") %>% 
#'  points(x=3, y=4, side=c(1,4), legend.name="Points 2", pch=5, col="red") %>% 
#'  legend(location="topright", title="LEGEND!!!")
#' topright
#' 
#' defaultLegend <- gsplot() %>% 
#'  points(x=1, y=2, side=c(3,2), legend.name="Points 1", pch=1, col="blue") %>%
#'  points(x=3, y=4, side=c(1,4), legend.name="Points 2", pch=5, col="red") %>%  
#'  legend()
#' defaultLegend
#'  
#' usrDef <- gsplot() %>% 
#'  points(x=1, y=2, side=c(3,2), legend.name="Points 1", cex=3) %>% 
#'  points(x=3, y=4, side=c(1,4), legend.name="Points 2", pch=5, col="red") %>% 
#'  lines(x=c(3,4,3), y=c(2,4,6), legend.name="Lines 1", lty=5, col="orange") %>%
#'  lines(x=c(1,2,5), y=c(1,8,5), legend.name="Lines 2", lwd=3) %>%  
#'  legend(x=1.5,y=4)
#' usrDef
#' 
#' gs <- gsplot() %>%
#'  points(1:4, 1:4, col=c("red","black","blue","green"), cex=1:4, pch=15:18) %>%
#'  legend(location = "topleft", pch=15:18,
#'        legend=c("a","b","c","d"),col="red", title = "Shape" ) %>%
#'  legend(x=1, y=2.5, pch=19, 
#'        legend=c("1","2","3","4"),col=c("red","black","blue","green"), title = "Color") %>%
#'  legend(location="bottomright",pch=19, legend=c("a1","a2","a3","a4"), 
#'        pt.cex = 1:4, title = "Size", col="red")
#' gs
legend <- function(object, ...){
  override("graphics", "legend", object, ...)
}


legend.gsplot <- function(object, ..., location="topright", legend_offset=0.3) {
  arguments <- list(...)
  
  if("x" %in% names(arguments)){
    location <- arguments$x
  }
  
  gsConfig <- list(location = location, legend_offset = legend_offset)
  
  arguments <- appendLegendPositionConfiguration(location, gsConfig, arguments)
  
  object <- append(object, list(legend = list(arguments = arguments, gs.config = gsConfig)))
  
  return(gsplot(object))
}

appendLegendPositionConfiguration <- function(location, gsConfig, arguments) {
  #TODO support explicit x/y coords
  legend_offset <- gsConfig$legend_offset
  
  if(location == "below") {
    return(append(arguments, list(x = "bottom", y = NULL, inset=c(0, -legend_offset), bty="n")))
  } else if(location == "above") {
    return(append(arguments, list(x = "top", y = NULL, inset=c(0, -legend_offset), bty="n")))
  } else if(location == "toright") {
    return(append(arguments, list(x = "right", y = NULL, inset=c(-legend_offset, 0), bty="n")))
  } else if(location == "toleft") {
    return(append(arguments, list(x = "left", y = NULL, inset=c(-legend_offset, 0), bty="n")))
  } else if("x" %in% names(arguments)){
    return(arguments)
  } else {
    return(append(arguments, list(x = location)))
  }
}

#' gsplot draw_legend
#'
#' Will cycle through  the gsplot, looking at the legend configuration (if one exists), and
#' create legend entries based on the points, lines, etc. contained in the gsplot.
#' 
#' @param gsplot the gsplot to render legend

draw_legend <- function(gsplot) {
  
  for(index in which(names(gsplot) %in% "legend")){
    legendParams <- gsplot[[index]][['arguments']]
    
    par(xpd=TRUE)
    
    if(!("legend" %in% names(legendParams))){
    
      if(!is.null(legendParams)) {
        smartLegend <- data.frame(text = character(), 
                                  symbol = numeric(), 
                                  color = character(), 
                                  line = numeric(), 
                                  size = numeric(),
                                  stringsAsFactors = FALSE)
        getLegendItem <- function(newText, newSymbol, newColor, newLine, newSize) { 
          if(!is.null(newText)) {
            return(data.frame(text = newText, 
                              symbol = newSymbol, 
                              color = newColor, 
                              line = newLine, 
                              size = newSize,
                              stringsAsFactors = FALSE))
          } else {
            return(NULL)
          }
        }
        
        #get legend entries for points
        pts_i <- which(names(gsplot) %in% 'points')
        for (i in pts_i){
          pts <- gsplot[[i]]
          
          if(all((c("pch","col","cex") %in% names(pts[['arguments']])))){
            smartLegend <- rbind(smartLegend, getLegendItem(pts[['gs.config']]$legend.name, pts[['arguments']]$pch, pts[['arguments']]$col, NA, pts[['arguments']]$cex))
          } else {
            pch <- ifelse("pch" %in% names(pts[['arguments']]), pts[['arguments']]$pch, par("pch"))
            col <- ifelse("col" %in% names(pts[['arguments']]), pts[['arguments']]$col, par("col"))
            cex <- ifelse("cex" %in% names(pts[['arguments']]), pts[['arguments']]$cex, par("cex"))
            smartLegend <- rbind(smartLegend, getLegendItem(pts[['gs.config']]$legend.name, pch, col, NA, cex))
          }
          
        }
        
        #get legend entries for lines
        lines_i <- which(names(gsplot) %in% c('lines','abline'))
        for (i in lines_i){
          lines <- gsplot[[i]]
          if(all((c("lty","col") %in% names(lines[['arguments']])))){
            smartLegend <- rbind(smartLegend, getLegendItem(lines[['gs.config']]$legend.name, NA, lines[['arguments']]$col, lines[['arguments']]$lty, 1))
          } else {
            lty <- ifelse("lty" %in% names(lines[['arguments']]), lines[['arguments']]$lty, par("lty"))
            col <- ifelse("col" %in% names(lines[['arguments']]), lines[['arguments']]$col, par("col"))
            smartLegend <- rbind(smartLegend, getLegendItem(lines[['gs.config']]$legend.name, NA, col, lty, 1))
          }
          
        }
        
        smartLegend <- unique(smartLegend)
        
        lineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        
        lineNums <- suppressWarnings(as.numeric(smartLegend$line))
        smartLegend$line[!is.na(lineNums)] <- lineTypes[lineNums+1][!is.na(lineTypes[lineNums+1])]
        
        if(nrow(smartLegend) > 0){
        
          #only include pch if we have a non-NA entry for points
          if(length(pts_i) > 0) {
            legendParams <- append(legendParams, list(
              pch=smartLegend$symbol,
              pt.cex=smartLegend$size
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
          
          #for above/below, dynamically set the number of columns
          location <- gsplot[['legend']][['gs.config']][['location']]
          if(location == "below" || location == "above") {
            itemsPerCol <- 3 #TODO load this from config
            cols <- NROW(smartLegend) %/% 3;
            if(NROW(smartLegend) %% 3 > 0) {
              cols <- cols + 1
            }
            legendParams <- append(legendParams, list(
              ncol=cols
            ))
          }
          legend(legendParams) 
        }
      }
    } else {
      legend(legendParams)
    }
  }
}

legend_adjusted_margins <- function(gsPlot) {
  defaults <- config("plot")
  defaultMargins <- c(3, 3, 3, 3) #default margins should come from config
  leftRightMarginMultiplier <- 2 #load in config?
  
  if(!is.null(gsPlot$legend)) {
    location <- gsPlot$legend$legend.gs.config$location
    legend_offset <- ceiling(1 / gsPlot$legend$legend.gs.config$legend_offset)
    if(location == "below") {
      mar <- c(defaultMargins[1] + legend_offset, defaultMargins[2], defaultMargins[3], defaultMargins[4])
    } else if(location == "above") {
      mar <- c(defaultMargins[1], defaultMargins[2], defaultMargins[3] + legend_offset, defaultMargins[4])
    } else if(location == "toright") {
      mar <- c(defaultMargins[1], defaultMargins[2], defaultMargins[3], defaultMargins[4] + (legend_offset * leftRightMarginMultiplier))
    } else if(location == "toleft") {
      mar <- c(defaultMargins[1], defaultMargins[2] + (legend_offset * leftRightMarginMultiplier), defaultMargins[3], defaultMargins[4])
    } else {
      mar <- defaultMargins
    }
  } else {
    mar <- defaultMargins
  }
  return(mar)
}

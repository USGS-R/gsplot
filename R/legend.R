#' gsplot legend
#'
#' If called with gsplot as first argument, will set the internal gsplot configuration
#' for legends. See \code{\link[graphics]{legend}} for more details.
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


legend.gsplot <- function(object, ..., location="topright", title="EXPLANATION", legend_offset=0.3) {
  arguments <- list(...)
  
#   current_list <- config("legend")  # grabbing yaml defaults
#   title <- current_list$title
#   location <- current_list$location
  
  if("x" %in% names(arguments)){
    location <- arguments$x
  }
  
  gsConfig <- list(location = location, legend_offset = legend_offset, title = title)
  
  arguments <- appendLegendPositionConfiguration(location, gsConfig, arguments)
  arguments <- append(arguments, list(title=title))
  
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
  
  oldXPD <- par()$xpd
  
  for(index in which(names(gsplot) %in% "legend")){
    
    legendParams <- gsplot[[index]][['arguments']]
    
    par(xpd=TRUE)
    
    if(!("legend" %in% names(legendParams))){
      
      if(!is.null(legendParams)) {
        
        smartLegend <- data.frame(row.names=names(formals(graphics::legend)), stringsAsFactors = FALSE)	
        
        for (i in 1:length(names(gsplot)) ) {
          
          plotElement <- names(gsplot)[[i]]
          
          params <- gsplot[[i]]$gs.config[which(names(gsplot[[i]]$gs.config) == "legend.name")]
          if (length(params) == 0 || is.null(params$legend.name)) {next}
          
          names(params)[which(names(params) %in% "legend.name")] <- 'legend' 
          params <- append(params, gsplot[[i]]$arguments[which(names(gsplot[[i]]$arguments) %in% names(formals(graphics::legend))[-c(1,2)])])
          type <- gsplot[[i]]$arguments$type
          
          if (plotElement == "points") {
            names(params)[which(names(params) %in% "bg")] <- 'pt.bg'
            names(params)[which(names(params) %in% "cex")] <- 'pt.cex'
            names(params)[which(names(params) %in% "lwd")] <- 'pt.lwd'
            if (!is.null(type) && type %in% c("l", "o", "b", "c", "s", "S", "h")) {
              if (all(!names(params) %in% c("lty"))) {params <- append(params, list(lty=par("lty")))}
            } 
          }
          if (plotElement == "lines" && !is.null(type) && type %in% c("p", "o", "b", "c")) {
            if (all(!names(params) %in% c("pch"))) {params <- append(params, list(pch=par("pch")))}
            params <- append(params, list(pt.lwd=params$lwd))
            if (type == "p") {params <- params[-which(names(params) %in% c('lty', 'lwd'))]}
            names(params)[which(names(params) %in% "bg")] <- 'pt.bg'
            names(params)[which(names(params) %in% "cex")] <- 'pt.cex'
          }  
          if (plotElement %in% c("rect", "polygon")) {
            names(params)[which(names(params) %in% "col")] <- 'fill'
          }
          
          ifelse(length(smartLegend) == 0, smartLegendNames <- row.names(smartLegend), smartLegendNames <- names(smartLegend))
          newsmartLegend <- match(smartLegendNames, names(params))
          newsmartLegend[which(!is.na(newsmartLegend))] <- params[newsmartLegend[which(!is.na(newsmartLegend))]]
          names(newsmartLegend) <- smartLegendNames
          
          smartLegend <- rbind(smartLegend, as.data.frame(newsmartLegend, stringsAsFactors = FALSE))
          
        }
        
        #take out any calls with all NA, and add overall legend calls from legendParams
        indices <- unlist(sapply(seq_len(length(smartLegend)), function(x) {!all(is.na(smartLegend[[x]]))}))
        legendParams <- append(legendParams, smartLegend[indices])
        
        # change any numeric linetypes to character
        lineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
        if (any(legendParams$lty %in% c(as.character(1:6)))) { 
          ltyIndices <- which(legendParams$lty %in% c(as.character(1:6)))
          legendParams$lty[ltyIndices] <- sapply(as.numeric(legendParams$lty[ltyIndices]), function(x) lineTypes[x+1])
        }
        
        #if density is specified, default should be "NULL"
        #if (!is.null(legendParams$density)) {legendParams$density[which(is.na(legendParams$density))] <- par("bg")}
        
        #for above/below, dynamically set the number of columns
        location <- gsplot[['legend']][['gs.config']][['location']]
        if(location == "below" || location == "above") {
          itemsPerCol <- 3 #TODO load this from config
          cols <- NROW(smartLegend) %/% 3;
          if(NROW(smartLegend) %% 3 > 0) {
            cols <- cols + 1
          }
          legendParams <- append(legendParams, list(ncol=cols))
        }
        
        legend(legendParams) 
        
      }
    } else {
      legend(legendParams)
    }
  }
  
  par(xpd=oldXPD)
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

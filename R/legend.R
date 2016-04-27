#' gsplot legend
#'
#' If called with gsplot as first argument, will set the internal gsplot configuration
#' for legends. See \code{\link[graphics]{legend}} for more details.
#'
#' @param object gsplot object
#' @param \dots Overall legend parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details 
#' Overall legend inputs:
#' 
#' \code{x, y} coordinates OR use \code{location} which is a character string indicating the legend location: above, toright, toleft, or below (see \code{\link[graphics]{legend}} for more)
#'
#' \code{bty, bg, box.lty, box.lwd, box.col, cex, xjust, yjust
#' x.intersp, y.intersp, adj, text.width, merge, trace, 
#' plot, ncol, horiz, title, inset, xpd, title.col
#' title.adj, seg.len}
#'
#' Parameter inputs for each graphics call (use inside of lines, points, curve, etc):
#' 
#' \code{legend.name, fill, col, border, lty, lwd, pch, angle, 
#' density, pt.bg, pt.cex, pt.lwd, text.col, text.font}
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
  
  object <- modify_legend(object, location = location, legend_offset = legend_offset, draw = TRUE, ...)

  return(object)
}

#' gsplot draw_legend
#'
#' Will cycle through  the gsplot, looking at the legend configuration (if one exists), and
#' create legend entries based on the points, lines, etc. contained in the gsplot.
#' 
#' @param gsplot the gsplot to render legend

draw_legend <- function(gsplot) {
  
  
  draw <- gsplot[['legend']][['gs.config']][['draw']]
  legend_args_exist <- any(grepl('legend.args', names(gsplot[['legend']])))
  if (is.null(draw) || !draw || !legend_args_exist){ return() }
  
  oldXPD <- par()$xpd
  oldBg <- par('bg')
  
  for(index in which(names(gsplot[['legend']]) %in% "gs.config")){
    
    par(xpd=TRUE)
    
    if("legend.args" %in% names(gsplot[['legend']])) {
    
      default.args <- formals(graphics::legend)
      legendParamsALL <- gsplot[['legend']][['legend.args']]
      
      overallLegendArgs <- appendLegendPositionConfiguration(gsplot[['legend']][['gs.config']])
      legendParamsALL <- append(legendParamsALL, overallLegendArgs)
      legendOrdered <- legendParamsALL[na.omit(match(names(default.args), names(legendParamsALL)))]

      #for above/below, dynamically set the number of columns
      location <- gsplot[['legend']][['gs.config']][['location']]
      if(location == "below" || location == "above") {
        itemsPerCol <- 3 #TODO load this from config
        cols <- length(legendOrdered$legend) %/% 3;
        if(length(legendOrdered$legend) %% 3 > 0) {
          cols <- cols + 1
        }
        legendOrdered$ncol <- ifelse(is.null(legendOrdered$ncol), cols, legendOrdered$ncol)
      }
  
      #set bg so that fill/border/etc args are correct, then evaluate any quoted list items
      if(any(names(overallLegendArgs) %in% c("bg"))) {
        par(bg=overallLegendArgs$bg)
      }
      legendComplete <- lapply(legendOrdered, function(x) {unname(sapply(x, function(x) {eval(x)}))})
    
    } else {
      legendComplete <- appendLegendPositionConfiguration(gsplot[['legend']][[index]])
    }
    
    legend(legendComplete)
    par(xpd=oldXPD)
    par(bg=oldBg)
  }

}

appendLegendPositionConfiguration <- function(gsConfig) {
  #TODO support explicit x/y coords
  legend_offset <- gsConfig$legend_offset
  location <- gsConfig$location
  gsConfig$legend_offset <- NULL
  gsConfig$location <- NULL
  
  if(location == "below") {
    return(append(gsConfig, list(x = "bottom", y = NULL, inset=c(0, -legend_offset), bty="n")))
  } else if(location == "above") {
    return(append(gsConfig, list(x = "top", y = NULL, inset=c(0, -legend_offset), bty="n")))
  } else if(location == "toright") {
    return(append(gsConfig, list(x = "right", y = NULL, inset=c(-legend_offset, 0), bty="n")))
  } else if(location == "toleft") {
    return(append(gsConfig, list(x = "left", y = NULL, inset=c(-legend_offset, 0), bty="n")))
  } else if("x" %in% names(gsConfig)){
    return(gsConfig)
  } else {
    return(append(gsConfig, list(x = location)))
  }
}

# What is this for?
# legend_adjusted_margins <- function(gsPlot) {
#   defaults <- config("plot")
#   defaultMargins <- c(3, 3, 3, 3) #default margins should come from config
#   leftRightMarginMultiplier <- 2 #load in config?
#   
#   if(!is.null(gsPlot$legend)) {
#     location <- gsPlot$legend$legend.gs.config$location
#     legend_offset <- ceiling(1 / gsPlot$legend$legend.gs.config$legend_offset)
#     if(location == "below") {
#       mar <- c(defaultMargins[1] + legend_offset, defaultMargins[2], defaultMargins[3], defaultMargins[4])
#     } else if(location == "above") {
#       mar <- c(defaultMargins[1], defaultMargins[2], defaultMargins[3] + legend_offset, defaultMargins[4])
#     } else if(location == "toright") {
#       mar <- c(defaultMargins[1], defaultMargins[2], defaultMargins[3], defaultMargins[4] + (legend_offset * leftRightMarginMultiplier))
#     } else if(location == "toleft") {
#       mar <- c(defaultMargins[1], defaultMargins[2] + (legend_offset * leftRightMarginMultiplier), defaultMargins[3], defaultMargins[4])
#     } else {
#       mar <- defaultMargins
#     }
#   } else {
#     mar <- defaultMargins
#   }
#   return(mar)
# }

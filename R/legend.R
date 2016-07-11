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
  
  default.args <- formals(graphics::legend)
  
  if ("legend" %in% names(gsplot)){
  
    # TODO rather than preserve individual pars we should par-scope this draw_legend call
    # and par scope each run of the for loop
    oldXPD <- par()$xpd
    oldBg <- par('bg')
    
    for (legend.name in names(gsplot[['legend']])) {
      
      par(xpd=TRUE)
      
      legend <- gsplot[['legend']][[legend.name]]
      if (legend$draw) {
        legend <- appendLegendColumnInfo(legend)
        legend <- appendLegendPositionConfiguration(legend)
        # set required legend argument to NA if not exists
        if (!"legend" %in% names(legend)) {
         legend$legend <- NA
        }
  
        #set bg so that fill/border/etc args are correct, then evaluate any quoted list items
        if (any(names(legend) %in% c("bg"))) {
          par(bg=legend$bg)
        }
        legend <- lapply(legend, function(x) {unname(sapply(x, function(x) {eval(x)}))})
        # clean out arguments not allowed by legend 
        legend <- legend[na.omit(match(names(default.args), names(legend)))]
        legend(legend)
        par(xpd=oldXPD)
        par(bg=oldBg)
      }
    }
  }
}

appendLegendPositionConfiguration <- function(legend) {
  #TODO support explicit x/y coords
  legend_offset <- legend$legend_offset
  location <- legend$location
  legend$legend_offset <- NULL
  
  if(location == "below") {
    return(append(legend, list(x = "bottom", y = NULL, inset=c(0, -legend_offset), bty="n")))
  } else if(location == "above") {
    return(append(legend, list(x = "top", y = NULL, inset=c(0, -legend_offset), bty="n")))
  } else if(location == "toright") {
    return(append(legend, list(x = "right", y = NULL, inset=c(-legend_offset, 0), bty="n")))
  } else if(location == "toleft") {
    return(append(legend, list(x = "left", y = NULL, inset=c(-legend_offset, 0), bty="n")))
  } else if("x" %in% names(legend)){
    return(legend)
  } else {
    return(append(legend, list(x = location)))
  }
}

#' Based on location set legend columns
#' 
#' @param legend to set columns on
appendLegendColumnInfo <- function(legend) {
  location <- legend[['location']]
  if (location == "below" || location == "above") {
    itemsPerCol <- 3 # TODO load this from config
    cols <- length(legend$legend) %/% itemsPerCol;
    if(length(legend$legend) %% itemsPerCol > 0) {
      cols <- cols + 1
    }
    legend$ncol <- ifelse(is.null(legend$ncol), cols, legend$ncol)
  }
  return(legend)
}


#don't use legend.name more than once
check_legend_name <- function(legend.name, prev_calls){
  if(!is.null(legend.name) && sum(prev_calls != 0)){ legend.name <- NULL }
  return(legend.name)
}

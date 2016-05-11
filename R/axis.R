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
#'    bgCol(col="lightgoldenrod") %>%
#'    axis(side=c(3,4),labels=FALSE) %>%
#'    legend("topright")
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),las=0) %>%
#'    axis(side=c(4), labels=FALSE) %>%
#'    axis(side=c(1,3), n.minor=4)
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:10, 1:10) %>%
#'    axis(1, at = seq(0,10,by=0.1),labels=FALSE, tcl=0.15) %>%
#'    axis(2, reverse=TRUE)
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:5, c(1,10,100,1000,10000), log="y") %>%
#'    axis(side=c(2,4), n.minor=4, las=1)
#' gs
#' 
#' gs <- gsplot() %>%
#'    lines(1:5, c(1,10,100,1000,10000), log="y", axes=FALSE) %>%
#'    axis(side=c(2,4), labels=FALSE, n.minor=4)
#' gs
#' 
#' gs <- gsplot(xaxs='r', yaxs='r') %>%
#'    lines(1:5, c(1,10,100,1000,10000), log="y", xaxt='n') %>%
#'    axis(side=c(2,4), labels=FALSE, n.minor=4)
#' gs
#' 
#' gs <- gsplot() %>% 
#'   points(runif(30, 1,5), runif(30,0.5,3.5)) %>% 
#'   axis(side=1, at=seq(1,5,by=0.25),las=3) %>%
#'   axis(side=c(3,4), labels=FALSE)
#' gs
#' 
#' usrDef <- gsplot(mar=c(4,4,4,4)) %>% 
#'   points(x=1, y=2, side=c(3,2), cex=3, xlab='cat',log='x') %>% 
#'   points(x=3:10,y=4:11, side=c(1,2), log='y')
#' usrDef
axis <- function(object, ...) {
  override("graphics", "axis", object, ...)
}

axis.gsplot <- function(object, ..., n.minor=0, tcl.minor=0.15, reverse=NULL) {
  
  fun.name <- "axis"
  
  user_args <- filter_arguments(fun.name = fun.name, ...)$call.args
  
  sides <- user_args[[fun.name]]$side
  user_args[[fun.name]]$side <- NULL
  
  for(i in sides){
    arguments1 <- list(arguments = append(list(side=i), user_args[[fun.name]]), 
                       gs.config=list(n.minor=n.minor, tcl.minor=tcl.minor, 
                                      reverse=reverse))
    to.gsplot <- list(arguments1)
    names(to.gsplot) <- fun.name
    
    object <- append(object, to.gsplot)
  }
  class(object) <- 'gsplot'
  return(object)
  
}

draw_axis <- function(gsplot, index.axis) {

  draw_axis_execute <- function(axisParams, n.minor, index){
    
    if(n.minor == 0){
      axis(axisParams)
    } else {
      axis(axisParams)
      n.minor <- n.minor + 1
      
      # Minor axis:

      #if user hasn't specified "at", calculate it
      if(is.null(axisParams$at)){
        at <- axTicks(axisParams$side)
      } else {
        at <- axisParams$at
      }
      
      newAT <- c()
      
      if(axisParams$side %% 2 == 0){
        logScale <- par()$ylog
      } else {
        logScale <- par()$xlog
      }
        
      if(logScale){
        spacing <- median(diff(log10(at)))
        at <- c(10^(log10(at[1])-spacing),at,10^(log10(at[length(at)])+spacing))
      } else {
        spacing <- median(diff(at))
        at <- c(at[1]-spacing,at,at[length(at)]+spacing)
      }
      
      for(i in 2:length(at)){
        newAT <- c(newAT, seq(at[i-1], length.out = n.minor, by= (at[i]-at[i-1])/n.minor))
      }
      
      axisParams$at <- newAT
      axisParams$tcl <- gsplot[[index]][['gs.config']]$tcl.minor
      axisParams$labels <- FALSE
      axis(axisParams)
    }
  }
  
  if(length(index.axis) == 1){
    axisParams <- gsplot[[index.axis]][['arguments']]
    n.minor <- gsplot[[index.axis]][['gs.config']]$n.minor
    draw_axis_execute(axisParams, n.minor, index.axis)
  } else {
    
    for(i in index.axis){
      axisParams <- gsplot[[i]][['arguments']]
      n.minor <- gsplot[[i]][['gs.config']]$n.minor
      draw_axis_execute(axisParams, n.minor, i)
    }
    
  }

}
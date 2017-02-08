#' gsplot background_color
#'
#' Adds color to the plot background. 
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @export
#' 
#' @examples
#' gs <- gsplot() %>%
#'    points(y=c(3,1,2), x=4:6, xlim=c(0,NA),legend.name="Points") %>%
#'    background_color(col="lightgrey") %>%
#'    lines( c(3,4,3), c(2,4,6), legend.name="Lines", side=c(3,4)) %>%
#'    legend(location="topleft")      
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:100, rnorm(100,mean=10000, sd=1000), log="y") %>%
#'    background_color(col="lightgrey")
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:100, rnorm(100,mean=10000, sd=1000), log="y") %>%
#'    background_color() #yaml specifies lightgrey
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:100, rnorm(100,mean=10000, sd=1000), log="y") %>%
#'    background_color("lightgoldenrod") 
#' gs
background_color <- function(object, ...) {
  override("gsplot", "background_color", object, ...)
}


background_color.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){

  fun.name='background_color'
  object <- gather_function_info(object, fun.name, ..., 
                                 legend.name=legend.name, side=side,where = "first")
  
  return(object)

}

#' create a background color
#' 
#' @param col color code or name, 
#' see \code{\link[grDevices]{colors}}, \code{\link[grDevices]{palette}}. 
#' Here NULL means color 0.
#' @export
#' @keywords internal
background_color.default <- function(col=NULL){
  
  if(par()$xlog){
    x1 <- 10^(par("usr")[1])
    x2 <- 10^(par("usr")[2])
  } else {
    x1 <- par("usr")[1]
    x2 <- par("usr")[2]  
  }
  
  if(par()$ylog){
    y1 <- 10^(par("usr")[3])
    y2 <- 10^(par("usr")[4])    
  } else {
    y1 <- par("usr")[3]
    y2 <- par("usr")[4]  
  }
  
  rect(x1,y1,x2,y2,col = col, border = NA)
  
}




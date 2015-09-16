#' gsplot bgCol
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
#'    lines( c(3,4,3), c(2,4,6), legend.name="Lines", side=c(3,4)) %>%
#'    legend(location="topleft") %>%
#'    bgCol(col="lightgrey") 
#'     
#' gs
#' 
#' gsPlain <- gsplot()%>%
#'    points(1:100, rnorm(100,mean=10000, sd=1000), log="y") %>%
#'    bgCol(col="lightgrey")
#' gsPlain
bgCol <- function(object, ...) {
  override("gsplot", "bgCol", object, ...)
}


bgCol.gsplot <- function(object, ..., side=c(1,2)){
  set_window_args(object, fun.name="bgCol", ..., legend.name=NULL, side=side, package='gsplot')
}

bgCol.default <- function(col,...){
  
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
  
  rect(x1,y1,x2,y2,col = col,...)
  
}




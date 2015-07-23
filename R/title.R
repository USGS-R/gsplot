#' gsplot title
#'
#' Adds a title to the plot. 
#' 
#' @details Additional graphical parameter inputs:
#' \itemize{
#'  \item{\code{main}} {character string that goes above the plot}
#'  \item{\code{sub}} {character string that goes below the plot}
#'  \item{\code{col.main, col.sub}} {color for the main title and subtitle, respectively}
#'  \item{\code{font.main, font.sub}} {numeric value specifying the font style (1=normal, 2=bold, 3=italic, 4=bold and italic)}
#'  \item{\code{cex.main, cex.sub}} {numeric value specifying the size of the main title and subtitle}
#' }
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#'  
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, col="blue", pch=18, legend.name="Points", xlab="Stuff")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines", ylab="Data!")
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1")
#' gsNew <- legend(gsNew, location="topleft",title="Awesome!")
#' gsNew <- title(gsNew, main="Great Graph", col.main="grey", font.main=2, cex.main=2)
#' gsNew
title <- function(object, ...) {
  override("graphics", "title", object, ...)
}


title.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("title")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(title = list(arguments = arguments, 
                                               gs.config=list(legend.name = legend.name, 
                                                              side = side))))
  return(gsplot(object))
}
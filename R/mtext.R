#' gsplot mtext
#'
#' mtext stuff
#' 
#' @details Add additional functionality to title.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return modified gsplot object 
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines", ylab="Data!")
#' gsNew <- text(gsNew, x=3.5, y=1.5, labels="Test") 
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1") 
#' gsNew <- legend(gsNew, "topleft",title="Awesome!")
#' gsNew <- title(gsNew, main="Great Graph")
#' gsNew <- mtext(gsNew, "More Stuff", side=3, line=2)
#' gsNew
mtext <- function(object, ...) {
  override("graphics", "mtext", object, ...)
}


mtext.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  current_list <- config("mtext")
  arguments <- list(...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])

  
  if(length(side) == 1){
    arguments <- append(arguments, list(side=side))
    if (side %% 2 == 0){
      side <- c(1, side)
    } else {
      side <- c(side, 2)
    }
  }
  
  object <- append(object,  list(mtext = list(arguments = arguments, 
                                             gs.config=list(legend.name = legend.name, 
                                                            side = side))))
  return(gsplot(object))
}
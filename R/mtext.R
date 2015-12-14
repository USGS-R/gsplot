#' gsplot mtext
#'
#' Allows text to be added to the plot margins.  See \code{\link[graphics]{mtext}} for more details.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs: 
#' \itemize{
#'  \item{\code{text}} {character string specifying the text to be placed in the margins}
#'  \item{\code{side}} {numeric, specifies the side of the graph to place the text - bottom(1), left(2), top(3), or right(4)}
#'  \item{\code{line}} {numeric, specifies the distance from the plot to place the text (in 'number of text lines')}
#' }
#'    
#' @export
#' @examples
#' gs <- gsplot()
#' gsNew <- points(gs, y=1, x=2, col="blue", pch=18, legend.name="Points")
#' gsNew <- lines(gsNew, c(3,4,3), c(2,4,6), legend.name="Lines", ylab="Data!")
#' gsNew <- text(gsNew, x=3.5, y=1.5, labels="Test") 
#' gsNew <- abline(gsNew, b=1, a=0, legend.name="1:1") 
#' gsNew <- legend(gsNew, location="topleft",title="Awesome!")
#' gsNew <- title(gsNew, main="Great Graph")
#' gsNew <- mtext(gsNew, text="More Stuff", side=2, line=3)
#' gsNew
mtext <- function(object, ...) {
  override("graphics", "mtext", object, ...)
}


mtext.gsplot <- function(object, ..., legend.name=NULL, side = 3){
  
  stopifnot(length(side) == 1)
  
  fun.name <- "mtext"
  def.funs <- graphics::mtext
  
  user_args <- function_args(name=fun.name, package="graphics", side=side, ...)
  
  to.gsplot <- list(list(arguments = c(do.call(set_args, c(fun.name, user_args)), axes=FALSE),  
                         gs.config = list(legend.name = legend.name, side = side,
                                          par=par_arguments(user_args, def.funs)))) %>% 
    setNames(fun.name)
  
  object <- append(object, to.gsplot)
    
  return(gsplot(object))
  
 # set_window_args(object, fun.name='mtext', ..., legend.name=legend.name, def.funs=graphics::mtext)
}

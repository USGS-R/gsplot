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
  fun.name <- "mtext"
  def.funs <- graphics::mtext
  
  user_args <- function_args(name=fun.name, package="graphics", side=side, ...)
  
  sides <- user_args$side
  user_args[["side"]] <- NULL
  
  for(i in seq_along(sides)){
    arguments1 <- append(list(side=sides[i]), user_args)
    
    if(length(user_args$text) == length(sides)){arguments1$text <- arguments1$text[i]}
    if(length(user_args$at) == length(sides)){arguments1$at <- arguments1$at[i]}
    
    to.gsplot <- list(list(arguments = do.call(set_args, c(fun.name, arguments1)),  
                           gs.config = list(legend.name = legend.name, side=sides[i],
                                            par=par_arguments(arguments1, def.funs)))) %>% 
      setNames(fun.name)
    
    object <- append(object, to.gsplot)
    object <- gsplot(object)
    
  }
  
  for(v in which(names(object) == "view")){
    if(length(object[[v]]) == 2 && any(names(object[[v]]) %in% "mtext")){
      object[[v]]$window$axes <- FALSE
    }
  }
  
  return(object)
  
 # set_window_args(object, fun.name='mtext', ..., legend.name=legend.name, def.funs=graphics::mtext)
}
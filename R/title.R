#' gsplot title
#'
#' Adds a title to the plot.  See \code{\link[graphics]{title}} for more details.
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
#' gs <- gsplot() %>%
#'       points(y=1, x=2, col="blue", pch=18, legend.name="Points", xlab="Stuff") %>%
#'       lines(c(3,4,3), c(2,4,6), legend.name="Lines", ylab="Data!") %>%
#'       abline(b=1, a=0, legend.name="1:1") %>%
#'       legend(location="topleft",title="Awesome!") %>%
#'       title(main="Great Graph", col.main="grey", font.main=2, cex.main=2)
#' gs
#' gs <- gsplot() %>%
#'       points(y=1, x=2) %>%
#'       title(main="Great Graph")
#' gs
title <- function(object, ...) {
  override("graphics", "title", object, ...)
}


title.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  to.gsplot <- set_args("title", ..., package = "graphics")
  
  to.gsplot <- list("title"=to.gsplot)
  
  object <- append(object, to.gsplot)
  class(object) <- 'gsplot'
  return(object)
  # set_window_args(object, fun.name='title', ..., legend.name=legend.name, side=side, def.funs=graphics::title)
}
#' Set or Query Graphical Parameters
#'
#' par can be used to set or query graphical parameters. 
#' Parameters can be set by specifying them as arguments 
#' to par in tag = value form, or by passing them as a list 
#' of tagged values. See \code{\link[graphics]{par}} for more details.
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See \code{\link[graphics]{par}}
#' 
#' 
#' @export
#' @examples
#' gs2 <- gsplot(new=TRUE,mar=c(5,4,1,2)) %>%
#'   points(1, 2) %>%
#'   bgCol(col="white") 
#' gs2
#' 
#' gs2 <- gsplot(new=TRUE, cex=1.2) %>%
#'   points(1, 2) %>%
#'   bgCol(col="white") %>%
#'   par(new=FALSE, mar=c(5,4,3,3), cex=2.1) 
#' gs2
par <- function(object, ...) {
  override("graphics", "par", object, ...)
}


par.gsplot <- function(object, ...){
  arguments <- list(...)
  
  if (length(arguments) == 0){
    # // this is ACCESS to par:
    return(object$global$par)
  }
  
  object <- modify_global_par(object, arguments)
  return(object)
}
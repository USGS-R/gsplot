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
  current_list <- config("par")
  arguments <- list(...)
  
  # // only add config list items if they aren't in ..., and aren't already set in par (i.e., don't reset them)
  indicesToAdd <- !(names(current_list) %in% names(arguments)) & !(names(current_list) %in% names(object[['par']]))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  if ("par" %in% names(object)){
    # // keep any par that shouldn't be overwritten. The rest are dropped/replaced
    cur.par <- names(object[['par']])
    keep.par <- cur.par[!cur.par %in% names(arguments)]
    arguments <- append(arguments, object[['par']][keep.par])
    object[['par']] <- NULL
  } 
  object <- append(object, list(par = arguments))
  return(gsplot(object))
}
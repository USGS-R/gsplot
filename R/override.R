#' @importFrom utils getFromNamespace
#' @importFrom stats setNames
#' @importFrom methods existsFunction
#' @export
#' @keywords internal
override <- function(package, name, object, ...) {
  if(!missing(object) && class(object) == "gsplot") {
    fun <- function(object, ...) {
      UseMethod(name, object)
    }
    fun(object, ...)
  } else {
    params <- function_args(package, name, object, ...)
    
    if(package == "gsplot"){
      do.call(paste0(name,".default"), params)
    } else {
      do.call(getFromNamespace(name, package), params)
    }
    
  }
}


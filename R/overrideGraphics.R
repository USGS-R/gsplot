overrideGraphics <- function(name, object, ...) {
  if(!missing(object) && class(object) == "gsplot") {
    fun <- function(object, ...) {
      UseMethod(name, object)
    }
    fun(object, ...)
  } else {
    params <- list(...)
 
    if (!missing(object)) {
      params <- append(object, params)
    }
    
    
    defFun <- getFromNamespace(ifelse(existsFunction(paste0(name,".default")), paste0(name,".default"), name), 'graphics')
    
    names(params)[which(names(params) == "")] <- names(formals(defFun))[which(names(params) == "")]
    
    do.call(getFromNamespace(name, "graphics"), params)
  }
}
overrideGraphics <- function(name, object, ...) {
  if(!missing(object) && class(object) == "gsplot") {
    fun <- function(object, ...) {
      UseMethod(name, object)
    }
    fun(object, ...)
  } else {
    params <- list(...)
 
    if (!missing(object)) {
      if (!is.null(names(object)))
        params <- append(object, params)
      else 
        params <- append(list(object), params)
    }
    customFuns <- c("error_bar_horizontal","error_bar_vertical","bgCol")
    base.package <- ifelse(name %in% customFuns, "gsplot", "graphics")
    defFun <- getFromNamespace(ifelse(existsFunction(paste0(name,".default")), paste0(name,".default"), name), base.package)
    
    names(params)[which(names(params) == "")] <- names(formals(defFun))[which(names(params) == "")]
    
    if(base.package == "gsplot"){
      do.call(paste0(name,".default"), params)
    } else {
      do.call(getFromNamespace(name, base.package), params)
    }
    
  }
}
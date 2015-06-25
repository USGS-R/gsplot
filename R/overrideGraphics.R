overrideGraphics <- function(name, object, ...) {
  if(!missing(object) && class(object) == "gsplot") {
    fun <- function(object, ...) {
      UseMethod(name, object)
    }
    fun(object, ...)
  } else {
    params <- list()
    if (!missing(object)) {
      params <- append(params, object)
    }
    params <- append(params, list(...))
    do.call(getFromNamespace(name, "graphics"), params)
  }
}
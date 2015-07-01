overrideGraphics <- function(name, object, ...) {
  if(!missing(object) && class(object) == "gsplot") {
    fun <- function(object, ...) {
      UseMethod(name, object)
    }
    fun(object, ...)
  } else {
    params <- list()
    params <- append(params, list(...))
    switch(name,
           points={
             if (!missing(object)) {
               params <- append(params, list(x=object))
             }
             if(!("type" %in% names(params))){
               params <- append(params, list(type="p"))
             }
           },
           lines={
             if (!missing(object)) {
               params <- append(params, list(x=object))
             }
             if(!("type" %in% names(params))){
               params <- append(params, list(type="l"))
             }
           },
           legend={
             if (!missing(object)) {
               params <- append(params, object)
             }              
           })
    do.call(getFromNamespace(name, "graphics"), params)
  }
}
#' @importFrom utils getFromNamespace
#' @importFrom stats setNames
#' @importFrom methods existsFunction
override <- function(package, name, object, ...) {
  if(!missing(object) && class(object) == "gsplot") {
    fun <- function(object, ...) {
      UseMethod(name, object)
    }
    fun(object, ...)
  } else {
    params <- graphics_params(package, name, object, ...)
    
    if(package == "gsplot"){
      do.call(paste0(name,".default"), params)
    } else {
      do.call(getFromNamespace(name, package), params)
    }
    
  }
}

graphics_params <- function(package, name, object, ...){
  params <- list(...)
  
  if (!missing(object)) {
    if (!is.null(names(object)))
      params <- append(object, params)
    else {
      params <- append(list(object), params)
    }
  } else {
    object = c() # replace w/ empty
  }
  
  if (length(params) == 0)
    return(list())
  
  # // is there a method for this class?
  defFun <- getS3method(name,class(object),optional=TRUE) # will be NULL when object is missing
  if (is.null(defFun)){
    defFun <- getFromNamespace(ifelse(existsFunction(paste0(name,".default")), paste0(name,".default"), name), package)
  }
  
  arg.names = names(formals(defFun))[which(!names(formals(defFun)) %in% names(params))]
  
  if (is.null(names(params))){
    # // all are unnamed
    if (arg.names[seq_len(length(params))][1] == "..."){
      # // special case where unnamed args go to ..., and should remain as characters (such as par("usr"))
      return(params)
    } 
    names(params) <- arg.names[seq_len(length(params))]
  } else {
    names(params)[which(names(params) == "")] <- arg.names[seq_len(sum(names(params) == ""))]
  }
  
  
  # // re-order
  sort.i <- seq_len(length(params))
  match.i <- match(names(params), names(formals(defFun)))
  sort.i[!is.na(match.i)] <- match.i[!is.na(match.i)]
  params <- params[sort(sort.i, index.return = TRUE)$ix]
  
  return(params)
}
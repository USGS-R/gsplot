#' get matched argument list for a given function
#' 
#' extracts and names user-specified arguments according to function defaults 
#' 
#' @param package the package to use to get the function from
#' @param name the function name
#' @param object the first argument, which may have a class to match functions to
#' @param \dots user arguments to be used for the list
#' @param use.default use different function name other than <function>.default (optional)
#' 
#' @keywords internal
function_args <- function(package, name, object, ..., use.default=paste0(name,'.default')){
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
    defFun <- getFromNamespace(ifelse(existsFunction(use.default), use.default, name), package)
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
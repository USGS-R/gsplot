
#' get the defaults for a function name 
#' 
#' parent package for function, and default functions for capturing arguments
#' 
#' @param fun.name a chacter vector (length 1) for the function name
#' @param out the exports you want (defaults to 'package' & 'def.funs')
#' @return a list if more than one output is selected, unlisted if length one
function_defaults <- function(fun.name, out=c('package','def.funs')){
  stopifnot(length(fun.name) == 1)
  
               
  fun <- fun.details[[fun.name]]
  if (is.null(fun)){
    fun <- list(package='graphics', def.funs=getFromNamespace(fun.name, "graphics"))
  }
  
  if (length(out) > 1){
    return(fun[c(out)])
  } else {
    return(fun[[c(out)]])
  }
}
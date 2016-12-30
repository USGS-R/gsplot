#' set argument list for a given function
#' 
#' extracts default arguments, user-specified arguments, and gsplot config arguments  
#' into a function-ready list.
#' 
#' @param fun.name the name of the function to generate an arg list for
#' @param custom.config logical
#' @param \dots user arguments to be used for the list
#' @param package the package to use to get the function from (defaults to 'graphics')
#' 
#' @keywords internal
set_args <- function(fun.name, ..., custom.config = FALSE, package='graphics'){
  
  config_args <- config(fun.name, custom.config = custom.config)
  user_args <- function_args(name=fun.name, package=package, ...)
  
  if(fun.name %in% c('points', 'lines')){
    xy_args <- xy.coords(x = user_args$x, y = user_args$y)
    user_args <- append_replace(user_args, xy_args)
  }
  
  indicesToAdd <- !(names(config_args) %in% names(user_args))
  arguments <- append(user_args, config_args[indicesToAdd])
  return(arguments)
}


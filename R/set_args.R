set_args <- function(fun.name, ...){
  
  current_list <- config(fun.name)
  arguments <- graphics_params(package = 'graphics', name=fun.name, ...)
  
  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  return(arguments)
}
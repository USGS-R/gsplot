
#' modify the global config field of the gsplot object
#' 
#' @param object a gsplot object
#' @param option.args a list of function calls
#' @return a modified object with the global config field changed
#' @keywords internal
modify_config <- function(object, option.args, field='global'){
  config.names <- names(object[[field]]$config)
  arguments <- option.args[names(option.args) %in% config.names]
  object[[field]]$config <- append_replace(object[[field]]$config, arguments)
  return(object)
}
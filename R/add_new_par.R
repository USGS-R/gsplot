#' add a new defaul par list to the gsplot object
#' 
#' @param object a gsplot object
#' @param field the name of the field add the par to 
#' (e.g., 'global','view.1.2','side.1', etc)
#' @return a modified gsplot object
add_new_par <- function(object, field){
  defaults <- list(c())
  if (field == 'global'){
    defaults <- config('par', custom.config = object[["global"]][["config"]]["config.file"])
  }
  if ('par' %in% names(object[[field]]))
    stop('par in ', field, ' already exists, cannot add it.', call. = FALSE)
  object <- modify_par(object, arguments=defaults, field)
  return(object)
}
#' pass view arguments into gsplot
#' 
#' supports configuration arguments (e.g., xlim)
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @examples 
#' gs <- gsplot() %>% 
#'          points(x=1:5, y=1:5, xlim=c(0,10)) %>% 
#'          view(ylim=c(0, 8.5))
#'                
#' ylim(gs)
#' @export
view <- function(object, ...) {
  UseMethod(generic = 'view', object = object)
}
#' @keywords internal
#' @export
view.gsplot <- function(object, ..., side=c(1,2)){
  fun.name <- NULL
  object <- gather_function_info(object, fun.name, ..., legend.name=NULL, side=c(1,2))
  return(object)
}
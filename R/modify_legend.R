
#' add function call to legend
#' 
#' @param object
#' @param fun.name
#' @param legend.name
#' @param \dots
#' @examples 
#' gs <- gsplot() %>% 
#'          points(x=1:5, y=1:5, legend.name = 'points 1')
#' gs
#' 
#' gsplot:::modify_legend('points', x=2:6, y=2:6, legend.name = 'points')
#' 
#' @keywords internal
modify_legend <- function(object, fun.name, legend.name, ...){
  # // do nothing
  return(object)
}

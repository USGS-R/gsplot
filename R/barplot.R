#' gsplot barplot
#'
#' Creates bar graph 
#' 
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @export
#' 
#' @examples
#' gs <- gsplot() %>%
#'    barplot(height = cbind(x = c(465, 91) / 465 * 100,
#'    y = c(840, 200) / 840 * 100,
#'    z = c(37, 17) / 37 * 100),
#'    beside = FALSE,
#'    width = c(465, 840, 37),
#'    col = c(1, 2),
#'    legend.name = c("A", "B"))   
#' gs
#' 
barplot <- function(object, ...) {
  override(package="graphics", name="barplot", object=object, ...)
}

barplot.gsplot <- function(object, ..., legend.name=NULL, side=c(1,2)){
  
  fun.name <- 'barplot'
  object <- gather_function_info(object, fun.name, ..., legend.name=legend.name, side=side)
  
  return(object)
}

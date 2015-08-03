#' gsplot is.gsplot
#'
#' Checks to see whether the object is valid for gsplot. 
#'
#' @param object gsplot object
#' 
#' @details Checks to see that the class of the object is \code{gsplot}, and 
#' that there is only one \code{par} arguement in the object list. 
#'    
#' @rdname is.gsplot
#' @export
#' @examples
#' gs <- gsplot() %>%
#'    points(x=1:5, y=1:5, legend.name="Stuff") %>%
#'    lines(2:6, y=2:6, xlim=c(0,10), ylim=c(0,10)) %>%
#'    lines(x=c(1.5,6), y=c(9,3), col="blue") %>%
#'    axis(side=c(3,4),labels=FALSE) %>%
#'    legend("topright")
#' gs
#' is.gsplot(gs)
#' 
#' gs <- gsplot() %>%
#'      par(col.axis="red") %>%
#'      points(y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),
#'          col="blue", pch=18, legend.name="Points", xlab="Index") %>%
#'      axis(side=c(3,4), labels=FALSE) %>%
#'      par(las=2)
#' gs
#' is.gsplot(gs)
is.gsplot <- function(object){
  
  arguments <- names(object)
  duplicates <- arguments[duplicated(arguments)]
  
  if (class(object) == "gsplot"){
    class <- "Valid"	
    
    if (!"par" %in% duplicates) {
      par_arguments <- "Valid"
    } else { par_arguments <- "Invalid" }
    
  } else { class <- "Invalid" }
  
  validity <- rbind(class, par_arguments)
  return(validity)
    
}

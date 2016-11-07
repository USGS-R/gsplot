#' gsplot
#'
#' Used to change the class of inputs to "gsplot".
#'
#' @param x list
#' @param config.file path to the file that will only be used for setting 
#' par in this one gsplot object. If NA (default), par is set by the global options set by
#' loadConfig().
#' @param theme path to the file that will only be used for setting 
#' the gsplot theme in this one gsplot object. If NA (default), there is no theme.
#' @param frame.plot a logical indicating whether a box should be drawn around the plot.
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return gsplot 
#' @export
#' @rdname gsplot
#' @examples
#' gsplot() 
gsplot <- function(x = NULL, ...) UseMethod("gsplot")

#' @rdname gsplot
#' @export
gsplot.default <- function(...,config.file=NA, theme=NA,frame.plot=TRUE) {
  object <- gsplot(list(global=list('config'=list(frame.plot=frame.plot,
                                                  config.file=!is.na(config.file),
                                                  theme=!is.na(theme)))))
  object <- add_new_par(object, 'global')
  if (length(list(...)) > 0){
    object <- par(object, ...)
  }
  return(object)
}

#' @rdname gsplot
#' @exportMethod gsplot
gsplot.list <- function(x){
  
  class(x) <- "gsplot"
  invisible(x) 
}

#' Summary of gsplot object
#'
#' Summary information
#'
#' @param object list
#' @param \dots additional parameters
#' @export
#' @examples 
#' gs <- gsplot() %>%
#'        points(1:10,1:10) %>%
#'        axis(side=1, at=seq(1,10,length.out=18),las=3) %>%
#'        axis(side=3, labels=FALSE) %>%
#'        grid(side=c(1,2),col="green") %>%
#'        grid(side=c(3,4))
#' summary(gs)
summary.gsplot <- function(object,...){
  
  view.info <- view_info(object)
  cat("Summary information of plotting object:\n")
  cat(nrow(view.info),"views:\n")
  for(i in seq_len(nrow(view.info))){
    cat("View:",i,"\nx side:", view.info$x[i], ",y side:", view.info$y[i], "\n")
    cat("xlim:",xlim(object, side=view.info$x[i]),",")
    cat("ylim:",ylim(object, side=view.info$y[i]))
    if(view.info$log[i] != ""){
      cat(",log:",view.info$log[i])
    }
    cat("\n")
  }
}


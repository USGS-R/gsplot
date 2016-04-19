
#' add function call to the overall legend
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
  if(is.null(legend.name)) {
    return(object)
  }
  
  
  
  # // set_legend_args stuff goes here
  return(object)
}

#' get the arguments that go into the legend for a single function call
#' 
#' @param object
#' @param fun.name
#' @param legend.name
#' @param .dots lazy_dots
#' @keywords internal
get_legend_args <- function(){
  # // do stuff
  # return(legend_args)
}

#' add the current function call legend info to the overall legend arguments
#' 
#' @param object
#' @param new_legend_args
#' @param .dots lazy_dots
#' @keywords internal
combine_legend_args <- function(object, new_legend_args, ...){
  
  if(!"legend" %in% names(gsplot)){
    default.args <- formals(graphics::legend)
    overall.legend <- c("x", "y", "bty", "bg", "box.lty", "box.lwd", "box.col", "cex",
                        "xjust", "yjust", "x.intersp", "y.intersp", "adj", "text.width", 
                        "merge", "trace", "plot", "ncol", "horiz", "title", "inset", 
                        "xpd", "title.col", "title.adj", "seg.len")  
    not.overall <- default.args[which(!names(default.args) %in% overall.legend)]
    legendParamsALL <- vector("list", length(not.overall))
    names(legendParamsALL) <- names(not.overall)
  } else {
    legendParamsALL <- gsplot[['legend']][['legend.args']]
  }
  
  orderedParams <- new_legend_args[match(names(legendParamsALL), names(new_legend_args))]    
  for (j in seq_along(legendParamsALL)) {
    legendParamsALL[[j]] <- c(legendParamsALL[[j]], orderedParams[[j]])
  }
  
  return(legendParamsALL)
}

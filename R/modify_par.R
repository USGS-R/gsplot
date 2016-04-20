global.only.par <- c("ask", "fig", "fin", "lheight", "mai", "mar", "mex", "mfcol", "mfrow", "mfg",
                     "new", "oma", "omd", "omi", "pin", "plt", "ps", "pty", "usr", "xlog", "ylog",
                     "ylbias")

read.only.par <- c("cin", "cra", "csi", "cxy", "din", "page")

#' modify par for global, a view, or a side
#' 
#' @param object a gsplot object
#' @param \dots settings for par
#' @param side NULL if par pertains to global, length 1 for side, length 2 for view
#' @return a modified gsplot object
modify_par <- function(object, ..., side=NULL){
  
  if (is.null(side)){
    # // is global change
  } else if (length(side) == 1){
    # // is side par change
  } else if (length(side) == 2){
    # // is view par change
  } else {
    stop(side, ' not supported')
  }
}

modify_global_par <- function(object, ...){
  
}
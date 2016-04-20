global.only.par <- c("ask", "fig", "fin", "lheight", "mai", "mar", "mex", "mfcol", "mfrow", "mfg",
                     "new", "oma", "omd", "omi", "pin", "plt", "ps", "pty", "usr", "xlog", "ylog",
                     "ylbias")

can.set <- names(par(no.readonly=TRUE))

#' modify par for global, side, or view
#' 
#' @param object a gsplot object
#' @param arguments settings for par
#' @return a modified gsplot object
#' @examples 
#' gs <- gsplot()
#' gs <- gsplot:::modify_par(gs, list(new=TRUE, cex=1.2))
#' names(gs$global$par)
#' gs <- gsplot:::modify_par(gs, list(new=TRUE, cex=1.4, omi=c(0,0,0,0)))
#' names(gs$global$par)
modify_par <- function(object, arguments, side=NULL){

  
  if (is.null(side)){
    field <- 'global'
    available.par <- can.set
  } else if (length(side) == 1){
    field <- as.side_name(side)
    available.par <- can.set[!can.set %in% global.only.par]
  } else if (length(side) == 2){
    field <- as.view_name(side)
    available.par <- can.set[!can.set %in% global.only.par]
  }
  
  if (!all(names(arguments) %in% available.par)){
    stop('cannot set ', paste(names(arguments)[!names(arguments) %in% available.par], collapse=', ' ), call. = FALSE)
  }
  
  object[[field]]$par <- append_replace(object[[field]]$par, arguments)
  return(object)
}

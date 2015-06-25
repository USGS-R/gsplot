

#' @title Load gsplot config
#'
#' @description Loads the config file into options which are
#'used elsewhere in the application
#'
#' @param filename string to custom file 
#'
#'@examples
#'loadConfig()
#'@export
loadConfig = function(filename) {
  
  if(missing(filename)){
    
#     graphTemplate <- list(
#       points = list(
#         pch=19,
#         lwd=1
#       ),
#       lines = list(
#         lty=1,
#         lwd=2
#       ),
#       axis = list(
#         xaxs="i",
#         yaxs="i", 
#         tcl=0.5,
#         mgp=c(3,1,0)
#       )
#     ) 
    
    
    
#     graphTemplate <- list(
#       pch=c(19,15,17,18,21,22,24,23),
#       xaxs="i",
#       yaxs="i",
#       tcl=0.5,
#       mgp=c(3,1,0),
#       lty=c(1,2,3,4,5,6,1,2 )
#     ) 
    graphTemplate <- list(
      pch=19,
      xaxs="i",
      yaxs="i",
      tcl=0.5,
      mgp=c(3,1,0),
      lty=1,
      lwd=2
    )
    
  } else {
    load(filename)
  }

  options("gsplot"=graphTemplate)
}


config <- function(type){
  
  loadConfig()

  type <- match.arg(type, c("points","lines","axis"))
  
  config_list <- options("gsplot")[[1]]
  
  if(type %in% c("points")){
    formalsNames <- names(formals(plot.xy))
    formalsNames <- formalsNames[formalsNames != "..."]
  }
  
  if(type %in% c("lines")){
    formalsNames <- names(formals(plot.xy))
    formalsNames <- formalsNames[formalsNames != "..."]
  }
  
  if(type %in% c("axis")){
    formalsNames <- names(formals(graphics::axis))
    formalsNames <- formalsNames[formalsNames != "..."]
  }
  
  current_list <- config_list[names(config_list) %in% formalsNames]

  return(current_list)
  
}


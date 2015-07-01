

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
      tcl=0.3,
      mgp=c(1.5,.3,0),
      lty=1,
      lwd=1,
      grid=list(lty=2,
                col="grey"),
      points=list(pch=6,col="red"),
      lines=list()
    )
    
  } else {
    load(filename)
  }
  options("gsplot"=graphTemplate)
}


config <- function(type=c("par","points","lines","axis","plot"),...){
  
  loadConfig()

  type <- match.arg(type)
  
  config_list <- options("gsplot")[[1]]
  
  globalConfig <- config_list[!(names(config_list) %in% c("points","lines","grid"))]
  
  if(type %in% c("par")){
    formalsNames <- names(par(no.readonly = TRUE))
    formalsNames <- formalsNames[formalsNames != "..."]
  }
  
  if(type %in% c("points")){
    formalsNames <- names(formals(plot.xy))
    formalsNames <- formalsNames[formalsNames != "..."]
    globalConfig[names(config_list$points)] <- NULL
    globalConfig <- append(globalConfig, config_list$points)
  }
  
  if(type %in% c("lines")){
    formalsNames <- names(formals(plot.xy))
    formalsNames <- formalsNames[formalsNames != "..."]
    globalConfig[names(config_list$lines)] <- NULL
    globalConfig <- append(globalConfig, config_list$lines)
  }
  
  if(type %in% c("plot")){
    formalsNames <- names(formals(plot.xy))
    formalsNames <- formalsNames[formalsNames != "..."]
  }
  
  if(type %in% c("axis")){
    formalsNames <- names(formals(graphics::axis))
    formalsNames <- formalsNames[formalsNames != "..."]
  }
  
  if(type %in% c("legend")){
    formalsNames <- names(formals(graphics::legend))
    formalsNames <- formalsNames[formalsNames != "..."]
  }
  
  if(type %in% c("grid")){
    formalsNames <- names(formals(graphics::grid))
    formalsNames <- formalsNames[formalsNames != "..."]
    globalConfig[names(config_list$grid)] <- NULL
    globalConfig <- append(globalConfig, config_list$grid)
  }
  
  globalConfig <- globalConfig[names(globalConfig) %in% formalsNames]
  
  globalConfig[names(list(...))] <- NULL
  globalConfig <- append(globalConfig, list(...))
  
  
  return(globalConfig)
  
}


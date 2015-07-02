

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
#' @importFrom graphics plot.xy
#' @importFrom graphics par
loadConfig = function(filename) {
  
  if(missing(filename)){

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
      lines=list(),
      abline=list(col="grey"),
      legend=list(),
      axis=list()
    )
    
  } else {
    load(filename)
  }
  options("gsplot"=graphTemplate)
}


config <- function(type,...){
  
  loadConfig()
  
  allowedTypes <- c("par","points","lines","axis","plot","abline","grid","legend")
  
  type <- match.arg(type, choices = allowedTypes)
  
  config_list <- options("gsplot")[[1]]
  
  globalConfig <- config_list[!(names(config_list) %in% allowedTypes[allowedTypes != "par"])]
  
  formalsNames <- names(formals(plot.xy))
  formalsNames <- switch(type,
                         par=names(par(no.readonly = TRUE)),
                         axis=names(formals(graphics::axis)),
                         legend=names(formals(graphics::legend)),
                         grid=names(formals(graphics::grid)),
                         abline=names(formals(graphics::abline)),
                         formalsNames)
  
  formalsNames <- formalsNames[formalsNames != "..."]
  
  globalConfig <- globalConfig[names(globalConfig) %in% formalsNames]
  
  if(type %in% names(config_list)){
    globalConfig[names(config_list[[type]])] <- NULL
    globalConfig <- append(globalConfig, config_list[[type]])
  }
  globalConfig[names(list(...))] <- NULL
  globalConfig <- append(globalConfig, list(...))
  
  return(globalConfig)
  
}




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
      points=list(pch=6,col="red"),
      lines=list(),
      abline=list(col="grey"),
      legend=list(),
      axis=list(at=NULL, 
                 labels=TRUE,
                 tick=TRUE, 
                 line=NA, 
                 pos=NA, 
                 outer=FALSE),
      title=list(),
      text=list(),
      mtext=list(),
      grid=list(col="grey",
                lwd=1, lty=2),
      segments=list(),
      error_bar=list(),
      arrows=list(),
      callouts=list(col='black'),
      bgCol=list()
    )
    
  } else {
    load(filename)
  }
  usrOptions <- do.call(c, unname(options("gsplot")))
  #Need to respect user options but add the template if not in user options
  
  if(any((names(graphTemplate) %in% names(usrOptions)))){
    for(type in names(graphTemplate)[names(graphTemplate) %in% names(usrOptions)]){
      graphTemplate[[type]] <- NULL
    }
    graphTemplate <- append(usrOptions, graphTemplate)
  }
  
  options("gsplot"=graphTemplate)
}


config <- function(type,...){
  
  loadConfig()
  
  allowedTypes <- c("par","points","lines","axis","plot",
                    "abline","legend","title","text",
                    "mtext","grid","segments",
                    "error_bar","arrows","bgCol","callouts")
  
  type <- match.arg(type, choices = allowedTypes)
  
  config_list <- options("gsplot")[[1]]
  
  globalConfig <- config_list[!(names(config_list) %in% allowedTypes[allowedTypes != "par"])]

  formalsNames <- names(formals(plot.xy))

  formalsNames <- switch(type,
                         par=names(par(no.readonly = TRUE)),
                         axis=names(formals(graphics::axis)),
                         legend=names(formals(graphics::legend)),
                         abline=names(formals(graphics::abline)),
                         title=names(formals(graphics::title)),
                         text=names(formals(graphics::text)),
                         mtext=names(formals(graphics::mtext)),
                         grid=names(formals(graphics::grid)),
                         segments=names(formals(graphics::segments)),
                         error_bar=names(formals(error_bar.default)),
                         bgCol=names(formals(bgCol.default)),
                         callouts=names(formals(callouts.default)),
                         formalsNames)
  
  formalsNames <- formalsNames[formalsNames != "..."]
  
  globalConfig <- globalConfig[names(globalConfig) %in% formalsNames]
  
  if(type %in% names(config_list)){
    globalConfig[names(config_list[[type]])] <- NULL
    globalConfig <- append(globalConfig, config_list[[type]])
  }
  # really goofy, but I couldn't find a way to test for list that doesn't fail when it is not a list
  if (length(expand.grid(...)) > 0 && is.list(list(...)[[1]])){ 
    globalConfig[names(...)] <- NULL
    globalConfig <- append(globalConfig, ...)
  } else {
    globalConfig[names(list(...))] <- NULL
    globalConfig <- append(globalConfig, list(...))
  }
  
  
  return(globalConfig)
  
}


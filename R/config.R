

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
#' @importFrom yaml yaml.load_file
loadConfig = function(filename) {
  
  if(missing(filename)){
    filename <- system.file("extdata", "default.yaml", package = "gsplot")
  }

  graphTemplate <- yaml.load_file(filename)

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

#' @title Get configuration for gsplot
#'
#' @description Gets config for gsplot, mostly used internally
#' but exposed for use by gsplot users
#'
#' @param type string of gsplot config object to retrieve
#' @param ... additional configuration to override what is pulled from config
#'
#' @examples
#' config("par")
#' 
#' @importFrom graphics plot.xy
#' @importFrom graphics par
#' @export
config <- function(type, ...){
  allowedTypes <- c("par","points","lines","axis","plot",
                    "abline","legend","title","text",
                    "mtext","grid","segments",
                    "error_bar","arrows","bgCol","callouts","orderToPlot")
  
  type <- match.arg(type, choices = allowedTypes)
  
  loadConfig()
  
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
                         orderToPlot='order',
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
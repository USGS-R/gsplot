gsconfig <- new.env(parent = emptyenv())

overrides <- list("par" = "par",
                  "points" = "plot.xy",
                  "lines" = "plot.xy",
                  "axis" = "axis",
                  "plot" = "plot.xy",
                  "abline" = "abline",
                  "legend" = "legend",
                  "title" = "title",
                  "text" = "text",
                  "mtext" = "mtext",
                  "grid" = "grid",
                  "segments" = "segments",
                  "error_bar" = "error_bar",
                  "arrows" = "arrows",
                  "bgCol" = "bgCol",
                  "callouts" = "callouts",
                  "rect" = "rect",
                  "polygon" = "polygon",
                  "symbols" = "symbols", 
                  "curve" = "curve")

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

  gsconfig$options <- graphTemplate
}

#' @title Get configuration for gsplot
#'
#' @description Gets config for gsplot, mostly used internally
#' but exposed for use by gsplot users
#'
#' @param type string of gsplot config object to retrieve
#' @param ... additional configuration to override what is pulled from config
#' @param persist logical of whether to persist overrides to config
#'
#' @examples
#' config("par")
#' 
#' @importFrom graphics plot.xy
#' @importFrom graphics par
#' @export
config <- function(type, ..., persist=FALSE){
  allowedTypes <- names(overrides)
  
  type <- match.arg(type, choices = allowedTypes)
  
  if (is.null(gsconfig$options)) {
    loadConfig()
  }
  
  config_list <- gsconfig$options
  
  globalConfig <- config_list[!(names(config_list) %in% allowedTypes[allowedTypes != "par"])]

  formalsNames <- formal_names(type)

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
  
  if (persist){
    if (type == "par"){
      gsconfig$options[names(globalConfig)] <- globalConfig 
    } else {
      gsconfig$options[[type]] <- globalConfig
    }
  }
  
  return(globalConfig)
}

formal_names <- function(type) {
  formals <- NULL
  
  if (type == "par") {
    formals <- names(par(no.readonly=TRUE))
  } else {
    func <- tryCatch(getFromNamespace(overrides[[type]], "graphics"), error=function(e){})
    
    if (!is.null(func)) {
      formals <- names(formals(func))
    } else {
      formals <- names(formals(overrides[[type]]))
    }
  }
  formals <- formals[formals != "..."]
  
  return(formals)
}
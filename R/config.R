gsconfig <- new.env(parent = emptyenv())
gsconfig$original.par <- par(no.readonly = TRUE)

#Question...how can I update the user's par?

#' @title Load gsplot config
#'
#' @description Loads the config file into options which are
#'used elsewhere in the application. This will only change the config paremeters while
#'building up the gsplot object, not on print.
#'
#' @param filename string to custom file 
#'
#'@examples
#'loadConfig()
#'@export
#' @importFrom graphics plot.xy
#' @importFrom grDevices dev.off
#' @importFrom graphics par
#' @importFrom yaml yaml.load_file
loadConfig = function(filename) {
  
  if(missing(filename)){
    filename <- system.file("extdata", "default.yaml", package = "gsplot")
  }

  graphTemplate <- yaml.load_file(filename)

  if(length(all.equal(gsconfig$original.par, par(no.readonly = TRUE))) > 1){
    par(gsconfig$original.par)
  }
  gsconfig$options <- graphTemplate
}

#' @title Load gsplot temporary config
#'
#' @description Loads the config file into options which are
#'used elsewhere in the application. This will only change the config paremeters while
#'building up the gsplot object, not on print.
#'
#' @param filename string to custom file 
#'
#' @importFrom graphics plot.xy
#' @importFrom graphics par
#' @importFrom yaml yaml.load_file
#' @importFrom grDevices dev.off
load_temp_config = function(filename) {
  
  graphTemplate <- yaml.load_file(filename)

  if(length(all.equal(gsconfig$original.par, par(no.readonly = TRUE))) > 1){
    par(gsconfig$original.par)
  }
  gsconfig$temp.config <- graphTemplate
}


#' @title Get configuration for gsplot
#'
#' @description Gets config for gsplot, mostly used internally
#' but exposed for use by gsplot users
#'
#' @param type string of gsplot config object to retrieve
#' @param ... additional configuration to override what is pulled from config
#' @param persist logical of whether to persist overrides to config
#' @param custom.config logical of whether to use default global (FALSE) or a config set for only one gsplot object 
#'
#' @examples
#' config("par")
#' 
#' @importFrom graphics plot.xy
#' @importFrom graphics par
#' @export
config <- function(type, ..., persist=FALSE, custom.config = FALSE){
  allowedTypes <- names(pkg.env$fun.details)
  
  type <- match.arg(type, choices = allowedTypes)
  
  if (is.null(gsconfig$options)) {
    loadConfig()
  }
  
  if(custom.config){
    config_list <- gsconfig$temp.config
  } else {
    config_list <- gsconfig$options
  }
  
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
    funs <- function_defaults(type, out='def.funs')
    if (length(funs) > 1){
      formals <- c_unname(lapply(funs, function(x) names(formals(x))))
      formals <- unique(formals)
    } else {
      formals <- names(formals(funs)) 
    }
  }
  formals <- formals[formals != "..."]
  
  return(formals)
}
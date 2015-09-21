#' set argument list for a given function
#' 
#' extracts default arguments, user-specified arguments, and gsplot config arguments  
#' into a function-ready list.
#' 
#' @param fun.name the name of the function to generate an arg list for
#' @param \dots user arguments to be used for the list
#' @param package the package to use to get the function from (defaults to 'graphics')
#' 
#' @keywords internal
set_args <- function(fun.name, ..., package='graphics'){
  
  config_args <- config(fun.name)
  user_args <- function_args(name=fun.name, package=package, ...)
  
  indicesToAdd <- !(names(config_args) %in% names(user_args))
  arguments <- append(user_args, config_args[indicesToAdd])
  return(arguments)
}

set_window_args <- function(object, fun.name, ..., legend.name=NULL, side=c(1,2), package='graphics', def.funs = getFromNamespace(paste0(fun.name,'.default'), package)){
  dots = separate_args(...)
  args = dots$args
  if (!is.null(args))
    arguments = set_args(fun.name, lazy_eval(args), package=package)
  else
    arguments = set_args(fun.name, package=package)
  e.fun = dots$e.fun
  to.gsplot <- list(list(arguments = append(formal_arguments(arguments, def.funs, names(config(fun.name))), window_arguments(arguments, def.funs)),
                         gs.config=list(legend.name = legend.name, side = side, par=par_arguments(arguments, def.funs)))) %>% 
    setNames(fun.name)
  
  object <- gsplot(append(object, to.gsplot)) # append initial call
  if (!is.null(e.fun)){
    for (i in seq_len(length(e.fun))){
      fun.name = names(e.fun)[i]
      embed.args = set_inherited_args(fun.name, arguments, e.fun[[i]])
      object <- do.call(fun.name, append(list(object=object), embed.args))
    }
  }
  return(object)
  
}
set_inherited_args <- function(fun.name, inherited.args, ..., package='gsplot'){
  # // shed non-formals
  inherited.args = function_args(package, fun.name, inherited.args, drop=TRUE)
  return(c(inherited.args, set_args(fun.name, ..., package = package)))
}

set_legend_args <- function(object, fun.name, ...) {
  paramsAll <- c(...)
  
  # if (is.null(params)) {next}
  
  names(paramsAll)[which(names(paramsAll)=='legend.name')] <- 'legend'
  default.args <- formals(graphics::legend)
  overall.legend <- c("x", "y", "bty", "bg", "box.lty", "box.lwd", "box.col",
                      "xjust", "yjust", "merge", "trace", "plot", "ncol",
                      "horiz", "title", "inset", "title.col", "title.adj", "xpd")  
  legend.params <- default.args[-which(names(default.args) %in% overall.legend)]
  
  usr.args <- paramsAll[which(names(paramsAll) %in% names(legend.params))] 
  
  
  if (fun.name == "points") {
    pt.names <- c("lwd","bg","cex")
    names.index <- which(names(usr.args) %in% pt.names)
    pt.names.index <- which(pt.names %in% names(usr.args))
    names(usr.args)[names.index] <- paste0("pt.", pt.names)[pt.names.index]
    
    fun.specific <- list(fill=NA,
                         col=par("col"),
                         border=NA,
                         lty=1,
                         pch=1,
                         density=NA,
                         text.width=1,
                         text.font=1)
    
  } else if (fun.name == "lines") {
    pt.names <- c("lwd","bg","cex")
    names.index <- which(names(usr.args) %in% pt.names)
    pt.names.index <- which(pt.names %in% names(usr.args))
    
    fun.specific <- list(fill=NA,
                         border=NA,
                         density=NA,
                         text.width=1,
                         text.font=1)
    
  }
  
  legend.params[match(names(usr.args), names(legend.params))] <- usr.args
  legend.params[match(names(fun.specific), names(legend.params))] <- fun.specific
  
  object <- append(object, list(legend.args=legend.params))
  #object[['legend']] <- append(object[['legend']], list(arguments = legend.params))
  
  object <- gsplot(object)
  return(object)
}

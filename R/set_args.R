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
  
  if(any(names(paramsAll) %in% "legend.name")) {
  
    names(paramsAll)[which(names(paramsAll)=='legend.name')] <- 'legend'
    default.args <- formals(graphics::legend)
    overall.legend <- c("x", "y", "bty", "bg", "box.lty", "box.lwd", "box.col", "cex","xjust", 
                        "yjust", "adj", "text.width", "merge", "trace", "plot", "ncol", 
                        "horiz", "title", "inset", "title.col", "title.adj", "xpd")  
    not.overall <- default.args[which(!names(default.args) %in% overall.legend)]

    type <- paramsAll[['type']]
    
    if (fun.name == "points") {
      pt.names <- c("lwd","bg","cex")
      names.index <- which(names(paramsAll) %in% pt.names)
      pt.names.index <- which(pt.names %in% names(paramsAll))
      names(paramsAll)[names.index] <- paste0("pt.", pt.names)[pt.names.index]
      
      if(!is.null(type) && type %in% c('l','b','o')){
          if(is.null(paramsAll$lwd)){paramsAll$lwd <- 1}
          if(is.null(paramsAll$lty)){paramsAll$lty <- 1}
          if(type=='l'){paramsAll$pch <- NA}
      } else if(!is.null(type) && type %in% c('c','h','s','S')){
          paramsAll$pch <- NA
          if(is.null(paramsAll$lwd)){paramsAll$lwd <- 1}
          if(is.null(paramsAll$lty)){paramsAll$lty <- 1}
      } else if(!is.null(type) && type %in% c('n')){
          paramsAll$pch <- NA
      } 
      
      fun.specific <- list(fill=quote(par("bg")),
                           border=quote(par("bg")),
                           lty=NA,
                           lwd=NA,
                           pch=1,
                           density=NA,
                           pt.bg=quote(par("bg")),
                           pt.cex=par("cex"),
                           pt.lwd=par("lwd"),
                           text.col=par("col"),
                           text.font=1)

    } else if (fun.name %in% c("lines", "abline", "arrows", "segments")) {
      
      if(!is.null(type) && type %in% c('p', 'b','o')){
          if(is.null(paramsAll$pch)){paramsAll$pch <- 1}
          if(is.null(paramsAll$pt.bg)){paramsAll$pt.bg <- quote(par("bg"))}
          if(is.null(paramsAll$pt.cex)){paramsAll$pt.cex <- par("cex")}
          if(is.null(paramsAll$pt.lwd)){paramsAll$pt.lwd <- par("lwd")}
          if(type=='p'){
            paramsAll$lty <- NA
            paramsAll$lwd <- NA
          }
      } else if(!is.null(type) && type %in% c('n')){
          paramsAll$lty <- NA
          paramsAll$lwd <- NA
      }
      
      fun.specific <- list(fill=quote(par("bg")),
                           border=quote(par("bg")),
                           lty=1,
                           lwd=1,
                           pch=NA,
                           density=NA,
                           pt.bg=NA,
                           pt.cex=NA,
                           pt.lwd=NA,
                           text.col=par("col"),
                           text.font=1)
      
    } else if (fun.name %in% c("polygon", "rect")) {
      names.index <- which(names(paramsAll) %in% c("col"))
      names(paramsAll)[names.index] <- "fill"
      
      fun.specific <- list(fill=quote(par("bg")),
                           border=par("fg"),
                           lty=NA,
                           lwd=NA,
                           pch=NA,
                           density=NA,
                           pt.bg=NA,
                           pt.cex=NA,
                           pt.lwd=NA,
                           text.col=par("col"),
                           text.font=1)
    }
    
    usr.args <- paramsAll[which(names(paramsAll) %in% names(not.overall))] 
    add.args <- fun.specific[!names(fun.specific) %in% names(paramsAll)]
    
    not.overall[match(names(usr.args), names(not.overall))] <- usr.args
    not.overall[match(names(add.args), names(not.overall))] <- add.args
    
    if(!is.character(not.overall$lty)){
      lineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
      not.overall$lty <- ifelse(is.numeric(not.overall$lty), lineTypes[not.overall$lty + 1], 
                                  as.character(not.overall$lty))
    }
    
    object <- append(object, list(legend.args=not.overall))
    object <- gsplot(object)
  }
  
  return(object)
}

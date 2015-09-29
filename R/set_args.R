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

set_legend_args <- function(object, fun.name, ..., legend.name) {
  if(is.null(legend.name)) {
    return(object)
  }
  
  paramsAll <- set_args(fun.name, list(...))

  paramsAll$legend <- legend.name
  default.args <- formals(graphics::legend)
  overall.legend <- c("x", "y", "bty", "bg", "box.lty", "box.lwd", "box.col", "cex","xjust", 
                      "yjust", "x.intersp", "y.intersp", "adj", "text.width", "merge", "trace", 
                      "plot", "ncol", "horiz", "title", "inset", "title.col", "title.adj", "xpd")  
  not.overall <- default.args[which(!names(default.args) %in% overall.legend)]
  
  fun.default <- list(fill=quote(par("bg")),
                      col=par("col"),
                      angle=45,
                      density=NA,
                      text.col=par("col"),
                      text.font=1,
                      seg.len=2)

  type <- paramsAll[['type']]
  if(!is.null(type)){
    type.name <- switch(type, p='p', b='bo', o='bo', l='lchsS', 
                        c='lchsS', h='lchsS', s='lchsS', S='lchsS', n='n')
    params.needed <- switch(type.name, 
                            p=list(pch=1, pt.bg=quote(par("bg")), pt.cex=par("cex"), pt.lwd=par("lwd"), lty=NA, lwd=NA),
                            bo=list(pch=1, pt.bg=quote(par("bg")), pt.cex=par("cex"), pt.lwd=par("lwd")),
                            lchsS=list(pch=NA, lty=1, lwd=1),
                            n=list(lty=NA, lwd=NA, pch=NA))
    paramsAll <- set_type_params(paramsAll, type.name, params.needed)
  }
  
  if (fun.name == "points") {
    pt.names <- c("lwd","bg","cex")
    names.index <- which(names(paramsAll) %in% pt.names)
    pt.names.index <- which(pt.names %in% names(paramsAll))
    names(paramsAll)[names.index] <- paste0("pt.", pt.names)[pt.names.index]
    
    fun.specific <- append(fun.default, list(border=quote(par("bg")),
                                             lty=NA,
                                             lwd=NA,
                                             pch=1,
                                             pt.bg=quote(par("bg")),
                                             pt.cex=par("cex"),
                                             pt.lwd=par("lwd")))

  } else if (fun.name %in% c("lines", "abline", "arrows", "segments")) {
    
    fun.specific <- append(fun.default, list(border=quote(par("bg")),
                                             lty=1,
                                             lwd=1,
                                             pch=NA,
                                             pt.bg=NA,
                                             pt.cex=NA,
                                             pt.lwd=NA))
    
  } else if (fun.name %in% c("polygon", "rect")) {
    names.index <- which(names(paramsAll) %in% c("col"))
    names(paramsAll)[names.index] <- "fill"
    
    fun.specific <- append(fun.default, list(border=par("fg"),
                                             lty=NA,
                                             lwd=NA,
                                             pch=NA,
                                             pt.bg=NA,
                                             pt.cex=NA,
                                             pt.lwd=NA))
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
  
  object[['legend']] <- append(object[['legend']], list(legend.args=not.overall))
  return(object)
}

set_type_params <- function(list, type.name, params){
  for(k in names(params)){
    if(type.name  == 'n' || is.null(list[[k]])){
      list[[k]] <- params[[match(k, names(params))]]
    } 
  }
  return(list)
}

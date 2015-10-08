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
  
  fun.default <- list(legend=legend.name,
                      fill=quote(par("bg")),
                      col=par("col"),
                      border=NA,
                      lty=NA,
                      lwd=NA,
                      pch=NA,
                      angle=45,
                      density=NA,
                      pt.bg=NA,
                      pt.cex=NA,
                      pt.lwd=NA,
                      text.col=par("col"),
                      text.font=1)

  type <- paramsAll[['type']]
  if(!is.null(type)){
    type.name <- switch(type, p='p', b='bo', o='bo', l='lchsS', 
                        c='lchsS', h='lchsS', s='lchsS', S='lchsS', n='n')
    params.needed <- switch(type.name, 
                            p=list(pch=1, pt.bg=quote(par("bg")), pt.cex=par("cex"), pt.lwd=par("lwd"), lty=NA, lwd=NA),
                            bo=list(pch=1, pt.bg=quote(par("bg")), pt.cex=par("cex"), pt.lwd=par("lwd"), lty=1, lwd=1),
                            lchsS=list(pch=NA, lty=1, lwd=1),
                            n=list(lty=NA, lwd=NA, pch=NA))
    paramsAll <- set_type_params(paramsAll, type.name, params.needed)
    if(type.name %in% c('p', 'lchsS')) {fun.name <- switch(type.name, p="points", lchsS="lines")}
  }
  
  usr.args <- paramsAll[which(names(paramsAll) %in% names(fun.default))]
 
  if (fun.name == "points") {
    pt.names <- c("lwd","bg","cex")
    names(usr.args) <- replace(names(usr.args), which(names(usr.args) %in% pt.names), 
                                paste0("pt.", pt.names[which(pt.names %in% names(usr.args))]))
    fun.specific <- list(border=quote(par("bg")),
                         pch=1,
                         pt.bg=quote(par("bg")),
                         pt.cex=par("cex"),
                         pt.lwd=par("lwd"))

  } else if (fun.name %in% c("lines", "abline", "arrows", "segments")) {
    fun.specific <- list(border=quote(par("bg")),
                         lty=1,
                         lwd=1)
    
  } else if (fun.name %in% c("polygon", "rect")) {
    names(usr.args) <- replace(names(usr.args), which(names(usr.args)=="col"), "fill")
    usr.args$lty <- NA #lty/lwd should always be NA for polygon & rectangles in the legend
    usr.args$lwd <- NA  
    fun.specific <- list(border=par("fg"))
  }
  
  usr.args <- usr.args[which(names(usr.args) %in% names(fun.default))]
  fun.all <- replace(fun.default, match(names(fun.specific), names(fun.default)), fun.specific)
  add.args <- fun.all[!names(fun.all) %in% names(usr.args)]
  fun.legend.args <- append(usr.args, add.args)  

  if(!is.character(fun.legend.args$lty)){
    lineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    fun.legend.args$lty <- lineTypes[fun.legend.args$lty + 1]
  }
  
  object[['legend']] <- append(object[['legend']], list(legend.args=fun.legend.args))
  return(object)
}

set_type_params <- function(list, type.name, params){
  for(k in names(params)){
    if(type.name == 'p' && k %in% c('lty', 'lwd') ||
       type.name == 'lchsS' && k %in% 'pch' ||
       type.name  == 'n' || is.null(list[[k]])){
        list[[k]] <- params[[match(k, names(params))]]
    } 
  }
  return(list)
}

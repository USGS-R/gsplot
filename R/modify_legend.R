
#' add function call to the overall legend
#' 
#' @param object
#' @param fun.name
#' @param legend.name
#' @param \dots
#' @examples 
#' gs <- gsplot() %>% 
#'          points(x=1:5, y=1:5, legend.name = 'points 1')
#' gs
#' 
#' gsplot:::modify_legend('points', x=2:6, y=2:6, legend.name = 'points')
#' 
#' @keywords internal
modify_legend <- function(object, fun.name, legend.name, ...){
  if(is.null(legend.name)) {
    return(object)
  }
  
  call.args <- call_arguments(fun.name, ...)[[1]] #exclude embedded
  fun.legend.args <- get_legend_args(fun.name, call.args, legend.name, ...)
  all.legend.args <- combine_legend_args(object, fun.legend.args)
  
  object[['legend']][['legend.args']] <- all.legend.args
  
  # // set_legend_args stuff goes here
  return(object)
}

#' get the arguments that go into the legend for a single function call
#' 
#' @param fun.name
#' @param call.args
#' @param legend.name
#' @param .dots lazy_dots
#' @keywords internal
get_legend_args <- function(fun.name, call.args, legend.name, ...){
  # // do stuff
  # return(legend_args)
  
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
  
  type <- call.args[['type']]
  if(!is.null(type)){
    type.name <- switch(type, p='p', b='bo', o='bo', l='lchsS', 
                        c='lchsS', h='lchsS', s='lchsS', S='lchsS', n='n')
    params.needed <- switch(type.name, 
                            p=list(pch=1, pt.bg=quote(par("bg")), pt.cex=par("cex"), pt.lwd=par("lwd"), lty=NA, lwd=NA),
                            bo=list(pch=1, pt.bg=quote(par("bg")), pt.cex=par("cex"), pt.lwd=par("lwd"), lty=1, lwd=1),
                            lchsS=list(pch=NA, lty=1, lwd=1),
                            n=list(lty=NA, lwd=NA, pch=NA))
    call.args <- set_type_params(call.args, type.name, params.needed)
    if(type.name %in% c('p', 'lchsS')) {fun.name <- switch(type.name, p="points", lchsS="lines")}
  }
  
  if (fun.name == "points") {
    pt.names <- c("lwd","bg","cex")
    names(call.args) <- replace(names(call.args), which(names(call.args) %in% pt.names), 
                                paste0("pt.", pt.names[na.omit(match(names(call.args), pt.names))]))
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
    names(call.args) <- replace(names(call.args), which(names(call.args)=="col"), "fill")
    call.args$lty <- NA #lty/lwd should always be NA for polygon & rectangles in the legend
    call.args$lwd <- NA  
    fun.specific <- list(border=par("fg"))
  }
  
  usr.args <- call.args[which(names(call.args) %in% names(fun.default))]
  fun.all <- replace(fun.default, match(names(fun.specific), names(fun.default)), fun.specific)
  add.args <- fun.all[!names(fun.all) %in% names(usr.args)]
  fun.legend.args <- append(usr.args, add.args)  
  
  if(!is.character(fun.legend.args$lty)){
    lineTypes <- c("blank", "solid", "dashed", "dotted", "dotdash", "longdash", "twodash")
    fun.legend.args$lty <- lineTypes[fun.legend.args$lty + 1]
  }
  
  return(fun.legend.args)
}

#' figure out the correct par args needed depending on the "type" 
#' 
#' @param list
#' @param type.name
#' @param params
#' @keywords internal
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

#' add the current function call legend info to the overall legend arguments
#' 
#' @param object
#' @param new_legend_args
#' @param .dots lazy_dots
#' @keywords internal
combine_legend_args <- function(object, new_legend_args, ...){
  
  if(!"legend" %in% names(object)){
    default.args <- formals(graphics::legend)
    overall.legend <- c("x", "y", "bty", "bg", "box.lty", "box.lwd", "box.col", "cex",
                        "xjust", "yjust", "x.intersp", "y.intersp", "adj", "text.width", 
                        "merge", "trace", "plot", "ncol", "horiz", "title", "inset", 
                        "xpd", "title.col", "title.adj", "seg.len")  
    not.overall <- default.args[which(!names(default.args) %in% overall.legend)]
    legend.args <- vector("list", length(not.overall))
    names(legend.args) <- names(not.overall)
  } else {
    legend.args <- object[['legend']][['legend.args']]
  }
  
  orderedParams <- new_legend_args[match(names(legend.args), names(new_legend_args))]    
  for (j in seq_along(legend.args)) {
    legend.args[[j]] <- c(legend.args[[j]], orderedParams[[j]])
  }
  
  return(legend.args)
}

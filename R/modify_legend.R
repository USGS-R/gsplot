
#' add function call to the overall legend
#' 
#' @param object
#' @param fun.name
#' @param legend.name
#' @param \dots
#' @examples 
#' gs <- gsplot() %>% 
#'          points(x=1:5, y=1:5, legend.name = 'points 1') %>% 
#'          legend()
#' gs
#' 
#' gs <- gsplot() %>% 
#'          points(x=1:2, y=1:2, col = c("red", "blue"), 
#'                 legend.name = c('points 1', 'points 2')) %>% 
#'          legend()
#' gs
#' 
#' @keywords internal
add_to_legend <- function(object, fun.name, legend.name, call.args, option.args, where){
  
    if(is.null(legend.name)) {
      return(object)
    }
    if(!exists("legend", object)) {object$legend <- list()}
    if(!exists("legend.auto", object$legend)) {object$legend$legend.auto <- create_empty_legend()}

    # add/add to legend$legend.auto
    if(length(legend.name) > 1){
      call.args.df <- as.data.frame(call.args, stringsAsFactors = FALSE)
      
      for(p in seq(nrow(call.args.df))) {
        call.args.list <- as.list(call.args.df[p,])
        fun.legend.args <- get_legend_args(fun.name, call.args.list, legend.name[p], option.args)
        object[['legend']][['legend.auto']] <- combine_legend_args(object, fun.legend.args, where=where)
      }
      
    } else {
      fun.legend.args <- get_legend_args(fun.name, call.args, legend.name, option.args)
      object[['legend']][['legend.auto']] <- combine_legend_args(object, fun.legend.args, where=where)
    }

  return(object)
}

#' get the arguments that go into the legend for a single function call
#' 
#' @param fun.name
#' @param call.args
#' @param legend.name
#' @param option.args args extracted out as options
#' @keywords internal
get_legend_args <- function(fun.name, call.args, legend.name, option.args){

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
  fun.specific <- list()
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
  
  usr.args <- c(call.args[which(names(call.args) %in% names(fun.default))], option.args[which(names(option.args) %in% names(fun.default))])
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
#' @param new.legend.args
#' @param legend.args.exist
#' @param .dots lazy_dots
#' @keywords internal
combine_legend_args <- function(object, new.legend.args, ..., where){
  
  if (missing(where)){
    where <- 'last'
  }
  where <- match.arg(where, c('last','first'))
  
  legend.args <- object[['legend']][['legend.auto']]
  
  orderedParams <- new.legend.args[match(names(legend.args), names(new.legend.args))]
  for (j in seq_along(legend.args)) {
    if (where == 'first'){
      legend.args[[j]] <- c(orderedParams[[j]], legend.args[[j]])  
    } else {
      legend.args[[j]] <- c(legend.args[[j]], orderedParams[[j]])  
    }
    
  }
  
  return(legend.args)
}

#' Set up an empty legend
#'
create_empty_legend <- function() {
  not.overall <- get_legend_arg_names(indiv = TRUE)
  legend <- vector("list", length(not.overall))
  names(legend) <- not.overall
  
  # add draw = FALSE as default
  legend$draw <- FALSE
  
  return(legend)
}

#' add legend configs
#' 
#' @param object
#' @keywords internal
modify_legend <- function(object, location="topright", legend_offset=0.3, draw=FALSE, ...){
  # // this should be shared between add_to_legend and legend
  # // check if legend exists, if not add it (someone could call legend before any legend.names)
  legend.config <- list(location = location, legend_offset = legend_offset, draw = draw, ...)

  arguments <- list(...)
  # auto is used when "legend" arg comes from "legend.name" in gsplot calls
  legend.index <- ifelse("legend" %in% names(legend.config),length(grep("legend.\\d+", names(object$legend))) + 1, "auto")
  
  if ("x" %in% names(arguments)){
    legend.config$location <- legend.config$x
    legend.config$x <- NULL
  }
  if (legend.index == 'auto') {
    # Merge new legend config into existing auto legend if it exists
    auto.legend <- object$legend$legend.auto
    # if draw is true, stay true
    legend.config$draw <- legend.config$draw || is.null(auto.legend) || auto.legend$draw
    auto.legend[names(legend.config)] <- legend.config
    legend.config <- auto.legend
  }
  object[['legend']][[paste0("legend.", legend.index)]] <- legend.config
  return(object)
}

#' get vector of legend arguments - overall or for each entry
#' 
#' @param overall logical indicating whether overall legend arguments should be returned
#' @param indiv logical indiciating whether arguments applicable to each legend entry should be returned
#' @keywords internal
get_legend_arg_names <- function(overall = FALSE, indiv = FALSE, names.args = names(formals(graphics::legend))){
  # default.args <- formals(graphics::legend)
  overall.legend.graphics <- c("x", "y", "bty", "bg", "box.lty", "box.lwd", "box.col", "cex",
                               "xjust", "yjust", "x.intersp", "y.intersp", "adj", "text.width", 
                               "merge", "trace", "plot", "ncol", "horiz", "title", "inset", 
                               "xpd", "title.col", "title.adj", "seg.len")  
  overall.legend.gsplot <- c('location', 'legend_offset', 'draw')
  overall.legend <- c(overall.legend.gsplot, overall.legend.graphics)
  not.overall <- names.args[which(!names.args %in% overall.legend)]
  return.args <- list(overall = overall.legend, indiv = not.overall)
  
  if(overall && !indiv){
    return.args <- return.args[['overall']]
  } else if (!overall && indiv){
    return.args <- return.args[['indiv']]
  } else if (!overall && !indiv) {
    return.args <- NULL
  }
  
  return(return.args)
}

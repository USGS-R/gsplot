
add_new_view <- function(object, view.name){
  view <- list(c())
  names(view) <- view.name
  view.i <- which_views(object)
  if (length(view.i) == 0)
    last.view.i <- 0
  else 
    last.view.i <- max(view)
  object <- append(object, view, after = last.view.i)
  return(object)
}

add_to_view <- function(object, call.args, side){
  view.name <- as.view_name(side)
  new.view <- !view.name %in% view_names(object)
  
  if (new.view)
    object <- add_new_view(object, view.name)
  
  object[[view.name]] <- append(object[[view.name]], call.args)
  return(object)
}
#' add function call to view
#' 
#' @param fun.name
#' @param \dots
#' @examples 
#' gs <- gsplot() %>% 
#'          points(x=1:5, y=1:5, xlim=c(0,10), ylim=c(0,10), 
#'                callouts(labels=c(rep(NA, 4), "oh")), 
#'                error_bar(y.high=1))
#' gs
#' 
#' gsplot:::call_arguments('points', x=2:6, y=2:6, ylim=c(-1, 11))
#' gsplot:::call_arguments('points', x=1:5, y=1:5, xlim=c(0,10), ylim=c(0,10), 
#'                callouts(labels=c(rep(NA, 4), "oh")))
#' @keywords internal
call_arguments <- function(fun.name, ...){
  dots <- separate_args(...)
  
  norm.args <- normal_arguments(fun.name, dots$args)
  embed.args <- embedded_arguments(fun.name, dots$e.fun, parent.args=norm.args[[fun.name]])
  view.fun.args <- append(norm.args, embed.args)
  return(view.fun.args)
}

#' get the arguments that go into the function call, stripping out others and adding config defaults
#' 
#' @param fun.name
#' @param .dots lazy_dots
#' @keywords internal
normal_arguments <- function(fun.name, .dots){

  fun.defaults <- function_defaults(fun.name)
  
  if (!is.null(.dots))
    arguments = set_args(fun.name, lazy_eval(.dots), package=fun.defaults$package)
  else
    arguments = set_args(fun.name, package=fun.defaults$package)
  
  args <- list(formal_arguments(arguments, fun.defaults$def.funs, keep.names = names(config(fun.name))))
  names(args) <- fun.name
  return(args)
}

embedded_arguments <- function(fun.name, embedded.dots, parent.args){

  args <- list()
  if (!is.null(embedded.dots)){
    for (i in seq_len(length(embedded.dots))){
      fun.name = names(embedded.dots)[i]
      package <- function_defaults(fun.name, out='package')
      embed.args = list(set_inherited_args(fun.name, parent.args, embedded.dots[[i]], package=package))
      names(embed.args) <- fun.name
      args <- append(args, embed.args)
    }
  }
  return(args)
}
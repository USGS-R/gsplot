
add_new_view <- function(object, view.name){
  if (view.name %in% view_names(object))
    stop(view.name, ' already exists, cannot add it.', call. = FALSE)
  view <- list(c())
  names(view) <- view.name
  
  last.view.i <- max(which_views(object), 0)
  object <- append(object, view, after = last.view.i)
  return(object)
}
#' add function call to view
#' 
#' @param object a gsplot object
#' @param call.args a named list of function calls
#' @param side a numeric vector of side(s) appropriate for creating a view name
#' @return a modified object with the function added to the proper view
#' @keywords internal
add_to_view <- function(object, call.args, side){
  view.name <- as.view_name(side)
  new.view <- !view.name %in% view_names(object)
  
  if (new.view){
    object <- add_new_view(object, view.name)
  }
  
  object[[view.name]] <- append(object[[view.name]], call.args)
  return(object)
}

#' extract the call arguments
#' 
#' extract the normal call arguments, and embedded function 
#' call arguments and return as lists named according to their 
#' rendering functions. 
#' 
#' @param fun.name the name of the rendering function
#' @param \dots arguments to \code{fun.name} or an embedded function 
#' within it. 
#' @return list with arguments. List is named according to function 
#' names. 
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
#' @param fun.name the name of the rendering function
#' @param .dots lazy_dots arguments
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

#' get the embedded arguments that go into the function call
#' 
#' @param fun.name the name of the rendering function
#' @param embedded.dots expressions to be evaluated within \code{parent.args} data
#' @param parent.args data that should be accessible when evaluating \code{embedded.dots}
#' @keywords internal
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
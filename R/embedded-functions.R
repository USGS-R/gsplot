is_in_package <- function(x){
  find(as.character(x), mode = 'function') == paste0('package:',packageName())
}


separate_args <- function(...){
  dots <- lazy_dots(...)
  embeds <- sapply(dots, function(x) is_in_package(x$expr[[1]]))
  if (sum(embeds) > 1)
    stop('only one embedded function is currently supported')
  
  embedded.funs <- dots[[which(embeds)]]
  dots[[which(embeds)]] <- NULL
  
  fun.name = as.character(embedded.funs$expr[[1]])
  embedded.funs$expr[[1]] <- NULL
  embedded.funs$expr
  return(list(args = dots, e.fun = fun.name, e.args = embedded.funs$expr))
}
is_in_package <- function(x){
  if (is.symbol(x))
    return(FALSE)
  
  isTRUE(find(as.character(x[[1]]), mode = 'function') == paste0('package:',packageName()))
}


separate_args <- function(...){
  
  dots <- lazy_dots(...)
  args = list(args=dots)

  if(length(args[[1]]) == 0)
    return()
  embeds <- unname(sapply(dots, function(x) is_in_package(x$expr)))
  
  if (sum(embeds) == 0)
    return(args)
  
  embedded.funs <- dots[which(embeds)]
  args$args = dots[which(!embeds)]
  fun.names = sapply(embedded.funs, function(x) as.character(x$expr[[1]]))
  args$e.funs = lapply(embedded.funs, function(x) {x$expr[[1]] <- NULL; return(x$expr)}) %>% 
    setNames(fun.names)
  return(args)
}
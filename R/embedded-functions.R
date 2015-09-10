is_in_package <- function(x){
  if (is.symbol(x))
    return(FALSE)
  
  isTRUE(find(as.character(x[[1]]), mode = 'function') == paste0('package:',packageName()))
}


separate_args <- function(...){
  
  dots <- lazy_dots(...)
  args = list(args=dots,e.fun=c(),e.args=c())

  embeds <- unname(sapply(dots, function(x) is_in_package(x$expr)))
  if (sum(embeds) > 1)
    stop('only one embedded function is currently supported')
  else if (sum(embeds) == 0)
    return(args)
  
  embedded.funs <- dots[[which(embeds)]]
  dots[[which(embeds)]] <- NULL
  args$args = dots
  args$e.fun = as.character(embedded.funs$expr[[1]])
  embedded.funs$expr[[1]] <- NULL
  args$e.args = embedded.funs$expr
  return(args)
}
par_arguments <- function(arguments, def.funs){
  
  args = arguments[!names(arguments) %in% names(formal_arguments(arguments, def.funs)) & names(arguments) %in% names(par(no.readonly=T))]
  if (length(args) > 0)
    return(args)
  else 
    NULL
}

formal_arguments <- function(arguments, def.funs){
  if (length(def.funs) > 1)
    formal_names <- unique(unlist(lapply(def.funs, function(x)names(formals(x)))))
  else 
    formal_names <- names(formals(def.funs))
  args = arguments[names(arguments) %in% formal_names]
  if (length(args) > 0)
    return(args)
  else 
    NULL
}

window_arguments <- function(arguments, def.funs){
  args = arguments[!names(arguments) %in% names(formal_arguments(arguments, def.funs)) & !names(arguments) %in% names(par(no.readonly=T))]
  if (length(args) > 0)
    return(args)
  else 
    NULL
}
par_arguments <- function(arguments, def.funs){
  
  args = arguments[!names(arguments) %in% names(formal_arguments(arguments, def.funs)) &
                     names(arguments) %in% names(par(no.readonly=T))]
  if (length(args) > 0)
    return(args)
  else 
    NULL
}

formal_arguments <- function(arguments, def.funs, keep.names=NULL){
  if (length(def.funs) > 1)
    formal.names <- unique(unlist(lapply(def.funs, function(x)names(formals(x)))))
  else 
    formal.names <- names(formals(def.funs))
  args = arguments[names(arguments) %in% formal.names | names(arguments) %in% keep.names]
  if (length(args) > 0)
    return(args)
  else 
    NULL
}

window_arguments <- function(arguments, def.funs){
  args = arguments[!names(arguments) %in% names(formal_arguments(arguments, def.funs)) & 
                   !names(arguments) %in% names(par(no.readonly=T)) &
                   !names(arguments) %in% names(formals(graphics::legend))]
  if (length(args) > 0)
    return(args)
  else 
    NULL
}
user.par <- names(par(no.readonly=TRUE))

global.only.par <- c("ask", "fig", "fin", "lheight", "mai", "mar", "mex", "mfcol", "mfrow", "mfg",
                     "new", "oma", "omd", "omi", "pin", "plt", "ps", "pty", "usr", "xlog", "ylog",
                     "ylbias")

side.par <- c('cex.axis', 'col.axis', 'cex.lab', 'col.lab', 'font.axis', 'mgp', 'xaxp', 'yaxp', 
              'tck', 'tcl', 'las', 'fg', 'xaxt', 'yaxt', 'adj', 'yaxs', 'xaxs','font.lab', 'ann')

view.par <- user.par[!(user.par %in% side.par) & !(user.par %in% global.only.par)]

#' modify par for global, side, or view
#' 
#' @param object a gsplot object
#' @param arguments settings for par
#' @param field where to modify par. A view name, a side name, or 'global'
#' @return a modified gsplot object
#' @examples 
#' gs <- gsplot()
#' gs <- gsplot:::modify_par(gs, list(new=TRUE, cex=1.2))
#' names(gs$global$par)
#' gs <- gsplot:::modify_par(gs, list(new=TRUE, cex=1.4, omi=c(0,0,0,0)))
#' names(gs$global$par)
#' @keywords internal
modify_par <- function(object, arguments, field='global'){
  object[[field]]$par <- append_replace(object[[field]]$par, arguments)
  return(object)
}

#' modify the par for a view
#' 
#' @param object a gsplot object
#' @param arguments settings for par
#' @param side a side vector that can be coerced into a view name
#' @param on.readly what to do when an argument (or arguments) can't be modified 
#' "stop", "skip", or "warning" are supported
#' @keywords internal
modify_view_par <- function(object, arguments, side, on.readonly = 'skip'){
  view.name <- as.view_name(side)
  arguments <- verify_par_args(arguments, available.par = view.par, on.readonly = on.readonly)
  modify_par(object, arguments, view.name)
}

#' modify the par for side(s)
#' 
#' @param object a gsplot object
#' @param arguments settings for par
#' @param side a length 1 or 2 side vector that can be coerced into side names
#' @param on.readly what to do when an argument (or arguments) can't be modified 
#' "stop", "skip", or "warning" are supported
#' @keywords internal
modify_side_par <- function(object, arguments, side, on.readonly = 'skip'){
  arguments <- verify_par_args(arguments, available.par = side.par, on.readonly = on.readonly)
  for (side.name in as.side_name(side)){
    object <- modify_par(object, arguments, side.name)
  }
  return(object)
}

#' modify global par
#' 
#' @param object a gsplot object
#' @param arguments settings for par
#' @param on.readly what to do when an argument (or arguments) can't be modified 
#' "stop", "skip", or "warning" are supported
#' @keywords internal
modify_global_par <- function(object, arguments, on.readonly = 'stop'){
  verify_par_args(arguments, available.par = user.par, on.readonly = on.readonly)
  modify_par(object, arguments, 'global')
}

#' verify par arguments against a vector of allowed names
#' 
#' @param arguments named parameters for par
#' @param available.par par names that are allowed to be modified by this call
#' @param on.readly what to do when an argument (or arguments) can't be modified 
#' "stop", "skip", or "warning" are supported
#' @keywords internal
verify_par_args <- function(arguments, available.par, on.readonly=c('stop','skip','warning')){
  on.readonly <- match.arg(on.readonly)
  
  can.set <- names(arguments) %in% available.par
  par.args <- arguments[can.set]
  
  if (!all(can.set) && on.readonly == 'stop'){
    stop('cannot set ', paste(names(arguments)[!can.set], collapse=', '), call. = FALSE)
  } else if (!all(can.set) && on.readonly == 'warning'){
    warning('cannot set ', paste(names(arguments)[!can.set], collapse=', '), 
         '; arguments will be ignored', call. = FALSE)
  }
  
  return(par.args)
}
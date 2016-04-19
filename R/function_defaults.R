#' get the defaults for a function name 
#' 
#' parent package for function, and default functions for capturing arguments
#' 
#' @param fun.name a chacter vector (length 1) for the function name
#' @param out the exports you want (defaults to 'package' & 'def.funs')
#' @return a list if more than one output is selected, unlisted if length one
function_defaults <- function(fun.name, out=c('package','def.funs')){
  stopifnot(length(fun.name) == 1)
  funs <- list('points'=
                 list(package='graphics', def.funs=c(graphics::plot.xy, graphics::points.default)),
               'lines' = 
                 list(package='graphics', def.funs=c(graphics::plot.xy, graphics::lines.default)),
               'bgCol' = 
                 list(package='gsplot', def.funs=gsplot:::bgCol.default),
               'callouts' = 
                 list(package='gsplot', def.funs=gsplot::callouts.default),
               'error_bar' = 
                 list(package='gsplot', def.funs=gsplot:::error_bar.default))
               
  
  if (!fun.name %in% names(funs)){
    funs <- append(funs, list(
      'default' = 
        list(package='graphics', def.funs=getFromNamespace(fun.name, "graphics"))))
    fun.name <- 'default'
  }
  if (length(out) > 1){
    return(funs[[fun.name]][c(out)])
  } else {
    return(funs[[fun.name]][[c(out)]])
  }
}
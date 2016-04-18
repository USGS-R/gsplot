
function_defaults <- function(fun.name, out=c('package','def.funs')){
  stopifnot(length(fun.name) == 1)
  funs <- list('points'=
                 list(package='graphics', def.funs=c(graphics::plot.xy, graphics::points.default)),
               'lines' = 
                 list(package='graphics', def.funs=c(graphics::plot.xy, graphics::lines.default)),
               'callouts' = 
                 list(package='gsplot', def.funs=gsplot::callouts.default),
               'error_bar' = 
                 list(package='gsplot', def.funs=gsplot:::error_bar.default), 
               'default' = 
                 list(package='graphics', def.funs=getFromNamespace(paste0(fun.name,'.default'), "graphics")))
  
  if (!fun.name %in% names(funs))
    fun.name <- 'default'
  
  if (length(out) > 1){
    return(funs[[fun.name]][c(out)])
  } else {
    return(funs[[fun.name]][[c(out)]])
  }
}
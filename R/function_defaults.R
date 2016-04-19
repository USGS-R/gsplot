
function_defaults <- function(fun.name, out=c('package','def.funs')){
  funs <- list('points'=
                 list(package='graphics',def.funs=c(graphics::plot.xy, graphics::points.default)),
               'lines' = 
                 list(package='graphics',def.funs=c(graphics::plot.xy, graphics::lines.default)),
               'callouts' = 
                 list(package='gsplot',def.funs=gsplot::callouts.default),
               'error_bar' = 
                 list(package='gsplot',def.funs=gsplot:::error_bar.default))
  
  
  if (length(out) > 1){
    return(funs[[fun.name]][c(out)])
  } else {
    return(funs[[fun.name]][[c(out)]])
  }
}
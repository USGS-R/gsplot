apply_view_function <- function(object, fun.name, ..., legend.name, side){
  arguments <- filter_arguments(fun.name, ...)
  
  call.args <- arguments$call.args
  option.args <- arguments$option.args
  extracted.args <- arguments$extracted.args
  object <- apply_extracted_args(object, extracted.args, side=side)
  object <- modify_side(object, c(call.args, option.args), side=side)
  
  object <- modify_side_par(object, option.args, side=side)
  object <- modify_view_par(object, option.args, side=side)
  object <- modify_legend(object, call.args, legend.name)
  object <- add_to_legend(object, fun.name, legend.name, call.args, option.args)
  object <- add_to_view(object, call.args, side=side)
  class(object) <- 'gsplot'
  return(object)
}
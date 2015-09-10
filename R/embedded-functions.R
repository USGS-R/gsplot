is_in_package <- function(x){
  find(as.character(x), mode = 'function') == paste0('package:',packageName())
}
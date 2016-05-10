
append_replace <- function(old.list, new.list){
  out.list <- old.list
  out.list[names(old.list) %in% names(new.list)] <- new.list[names(out.list[names(old.list) %in% names(new.list)])]
  new.list[names(new.list) %in% names(old.list)] <- NULL
  out.list <- append(out.list, new.list)
  return(out.list)
}



c_unname <- function(list){
  unname(do.call(c, list))
}

unname_c <- function(list){
  do.call(c, unname(list))
}



remove_field <- function(list, param){
 
  for (v in param){
    if (v %in% names(list))
      list[[v]] <- NULL
  }
  return(list)
}

strip_pts <- function(list, param){
  out <- c()
  for (v in param){
    if (v %in% names(list) && !inherits(list[[v]], c('function','formula')))
      out <- append(out, list[[v]])
    else{
      if (any(sapply(list, is.list))){
        u.list <- unname_c(list[sapply(list, is.list)])
        if(v %in% names(u.list))
          out <- append(out, u.list[[v]])
        else if (any(sapply(u.list, function(x) any(names(x) %in% v))))
          out <- append(out, u.list[[which(sapply(u.list, function(x) any(names(x) %in% v)))]][[v]])
        else
          out <- append(out, NA)
      } else
        out <- append(out, NA)
      
    }
    
  }
  return(out)
}

strip_pts2 <- function(data, param) {
  out <- c()
  if (is.null(names(data))) {
    out <- append(out, unname_c(data))
  }
  for (name in param) {
    if (name %in% names(data)) {
      if (is.list(data[[name]])) {
        out <- append(out, strip_pts2(data[[name]], param))
      } else {
        out <- append(out, data[[name]])
      }
    }
  }
  return(out)
}


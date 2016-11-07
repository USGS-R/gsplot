
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
  out.class <- 'numeric'
  for (v in param){
    if (v %in% names(list) && !inherits(list[[v]], c('function','formula'))) {
      v.vals <- list[[v]]
      # append to out without losing timezones when it is a date/POSIX
      if(is.null(out)){
        out <- v.vals
      } else {
        out <- append(out, v.vals)
      }
      out.class <- ifelse(!all(is.na(v.vals)), class(v.vals), out.class)
    } else {
      if (any(sapply(list, is.list))){
        u.list <- unname_c(list[sapply(list, is.list)])
        if(v %in% names(u.list)) {
          v.vals <- u.list[[v]]
          out <- append_keepclass(out, v.vals)
          out.class <- ifelse(!all(is.na(v.vals)), class(v.vals), out.class)
        } else if (any(sapply(u.list, function(x) any(names(x) %in% v)))) {
          v.vals <- u.list[[which(sapply(u.list, function(x) any(names(x) %in% v)))]][[v]]
          # append to out without losing timezones when it is a date/POSIX
          out <- append_keepclass(out, v.vals)
          out.class <- ifelse(!all(is.na(v.vals)), class(v.vals), out.class)
        } else {
          out <- append(out, NA)
        }
      } else
        out <- append(out, NA)
    }
    
  }
  class(out) <- out.class
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

# append without losing timezones for the case where 
# append.vals is POSIX and base.vals is NULL
append_keepclass <- function(base.vals, append.vals){
  if(is.null(base.vals)){
    vals <- append.vals
  } else {
    vals <- append(base.vals, append.vals)
  }
  return(vals)
}

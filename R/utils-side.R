#' names of the sides in gsplot object
#' 
#' @param object a gsplot object
#' @return a character vector of side names
#' @keywords internal
side_names <- function(object){
  names(sides(object))
}

#' take a view name and extract the numeric x side
#' 
#' @param view.name a chracter vector of view names
#' @return an integer vector of x sides
#' @keywords internal
as.x_side <- function(view.name){
  unname(sapply(view.name, function(x) as.numeric(strsplit(x,'[.]')[[1]][2])))
}

#' take a view name and extract the named x sides
#' 
#' @param view.name a chracter vector of view names
#' @return an character vector of x side names
#' @keywords internal
as.x_side_name <- function(view.name){
  as.side_name(as.x_side(view.name))
}

#' take a view name and extract the numeric y sides
#' 
#' @param view.name a chracter vector of view names
#' @return an integer vector of y sides
#' @keywords internal
as.y_side <- function(view.name){
  unname(sapply(view.name, function(x) as.numeric(strsplit(x,'[.]')[[1]][3])))
}

#' take a view name and extract the named y sides
#' 
#' @param view.name a chracter vector of view names
#' @return an character vector of s side names
#' @keywords internal
as.y_side_name <- function(view.name){
  as.side_name(as.y_side(view.name))
}

#' get the side names
#' 
#' @param x a numeric vector of sides or character vector of view names
#' @return a character vector of side names
#' @keywords internal
#' @export
as.side_name <- function(x) UseMethod("as.side_name")

#' @keywords internal
#' @export
as.side_name.numeric <- function(x){
  paste('side.',x, sep='')
}
#' @keywords internal
#' @export
as.side_name.character <- function(x){
  as.side_name(unname(sapply(x, function(x) as.numeric(tail(strsplit(x,'[.]')[[1]],-1L)))))
}

#' take a named side and turn it into numeric
#' 
#' @param side.names vector of side names
#' @return numeric values for \code{side}
#' @keywords internal
as.side <- function(side.names){
  as.numeric(gsub('side.','',side.names))
}

#' take a named or numbered side and give whether it is an x or y axis
#'
#' @param side numeric or named \code{side}
#' @return "x" or "y" character vector
#' @keywords internal
as.axis <- function(side) {
  axis <- rep("x", length(side))
  is.y <- as.side(side) %% 2 == 0
  axis[is.y] <- "y"
  return(axis)
}

#' which indices in the gsplot object are sides
#' 
#' @param gsplot a gsplot object
#' @return indices of sides
#' @keywords internal
which_sides <- function(gsplot){
  grep('side.', names(gsplot))
}

#' subset the gsplot object to return only sides
#' 
#' @param gsplot a gsplot object
#' @param by.index retrieve specific indices (NA returns all)
#' @return a sides list
#' @keywords internal
sides <- function(gsplot, by.index=NA){
  mySides <- gsplot[which_sides(gsplot)]
  if (!all(is.na(by.index))) {
    indices <- as.side(names(mySides))
    mySides <- mySides[indices %in% by.index]
  }
  return(mySides)
}

#' sets the user-defined limits to sides
#' 
#' @param lims the usr.lims
#' @param side a side
#' @return the modified side
#' @keywords internal
set_usr_lim <- function(lims, side){
  to.set <- !is.na(lims)
  side$lim[to.set] <- lims[to.set]
  side$usr.lim[to.set] <- TRUE
  return(side)
}

#' which sides are locked?
#' 
#' let's you know what sides have been user-defined
#' @param sides a sides list
#' @return names of the sides that are locked
#' @keywords internal
locked_sides <- function(sides){
  lim.locks <- sapply(sides, function(x) all(x$usr.lim))
  names(lim.locks)[lim.locks]
}

#' set the log value on a side
#' 
#' @param args arguments to pull log info from
#' @param side the side for the gsplot object (see \code{\link{sides}})
#' @return a modified \code{side}
#' @keywords internal
set_side_log <- function(args, side, side.num){
  stopifnot(length(side.num) == 1)
  axis <- as.axis(side.num)
  log <- ifelse(exists("log", args), args$log, "")
  if (log == "")
    return(side) # // do nothing
  side$log <- grepl(axis,log) || side$log
  return(side)
}

#' set the label value on a side
#' 
#' @param args arguments to pull label from
#' @param side the side for the gsplot object (see \code{\link{sides}})
#' @param side.num which side are we talking about
#' @return a modified \code{sides} list
#' @keywords internal
set_side_lab <- function(args, side, side.num){
  stopifnot(length(side.num) == 1)
  axis <- as.axis(side.num)
  lab.arg <- paste0(axis, "lab")
  if (exists(lab.arg, args)) {
    lab <- args[[]]
    side$label <- ifelse(is.expression(lab) || !is.na(lab), lab, side$label)
  }
  return(side)
}

#' sets the side limits according to a new addition
#' 
#' @param args pass through args from graphics call
#' @param side the side for the gsplot object (see \code{\link{side}})
#' @param side.num number of side being worked on
#' @return a modified \code{sides} list
#' @keywords internal
set_side_lim <- function(args, side, side.num){
  stopifnot(length(side.num) == 1)
  axis <- as.axis(side.num)
  include <- switch(axis,
                    x = c('x','x1','x0','xleft','xright', 'data'),
                    y = c('y','y1','y0','ytop','ybottom', 'data'))
  # // need value arguments, need yaxs/xaxs args, need user-specified ylim/xlim values
  usr.lims <- c(NA, NA)
  lim.arg = paste0(axis,"lim")
  if (exists(lim.arg, args)) {
    usr.lims <- args[[lim.arg]]
  }
  side <- set_usr_lim(usr.lims, side)
  side.vals <- strip_pts(args, include)
  
  if (any(!is.na(side.vals))){
    data.range <- range(c(side.vals[is.finite(side.vals)], side$lim), na.rm = TRUE)
    free.lim <- !side$usr.lim
    data.range[!free.lim] <- side$lim[!free.lim]
    side$lim <- data.range
  }
  return(side)
}

#' unlist and summarize values according to their side
#' 
#' @param view a single view to be used (named list, e.g., list(view.1.2=...))
#' @param na.value what to return when a \code{param} has no values
#' @param axis 'x' or 'y'
#' @param ignore fields in the \code{view} to ignore
#' @param skip.side a vector of side names to leave out 
#' (if, for example, they are locked and it doesn't matter what they contain)
#' @return a list of values for \code{param} indexed by 'side' names
#' @keywords internal
summarize_side_values <- function(view, param, na.value=NULL, axis=c('x','y'), ignore='gs.config', skip.side=NA){
  axis <- match.arg(axis)
  side_i <- c('x'=1,'y'=2)
  
  view_nm <- names(view)[which_views(view)] #// is it a view? if not, pass through
  if (length(view_nm) == 0){
    return(na.value)
  }
  
  side <- as.side_name(view_nm)[side_i[[axis]]]
  
  if (side %in% skip.side)
    return(na.value)
  
  x <- view[!names(view) %in% ignore]
  valStuff <- lapply(x, function(x) strip_pts(x[[1]], param))
  if (length(valStuff) == 1 & is.na(valStuff))
    return(na.value)
  values <- list(c_unname(valStuff)) %>% 
    setNames(side)
  return(values)
}

#' add sides list to gsplot object
#' 
#' @param gsplot a gsplot object
#' @param sides integer vector of sides to add
#' @param on.exist what to do when the sides already exists
#' @return a modified gsplot object
#' @keywords internal
append_sides <- function(gsplot, sides, on.exists = c('skip','replace')){
  
  if (is.null(sides))
    return(gsplot)
  on.exists = match.arg(on.exists)
  
  side_template <- list(lim = c(NA, NA), log=FALSE, label="", usr.lim=c(FALSE, FALSE))
  
  if (on.exists == 'skip'){
    sides <- as.side_name(sides)
    
    to_add <- !sides %in% names(gsplot)
    side_list <- rep(list(side_template), sum(to_add))
    side_list <- setNames(side_list, sides[which(to_add)])
    gsplot <- append(gsplot, side_list)
  } else if (on.exists == 'replace'){
    stop('on.exists ', on.exists, ' not implemented yet')
  }
  return(gsplot)
}

#' automatically fill in sides as pairs if they are single vals
#' 
#' @param sides an integer vector of length one or two 
#' @return a side integer vector of length two
#' @keywords internal
set_sides <- function(sides){
  if (length(sides)==1){
    if(sides %% 2 == 0)
      sides = c(1,sides)
    else 
      sides = c(sides,2)
  } 
  return(sides)
}

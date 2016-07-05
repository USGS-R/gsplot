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
  unname(sapply(view.name, function(x) get_sides_x(as.side(x))))
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
  unname(sapply(view.name, function(x) get_sides_y(as.side(x))))
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
#' @param x a character vector of side of view names (length 1)
#' @return numeric values for \code{side}
#' @keywords internal
as.side <- function(x){
  c_unname(lapply(x, function(x) as.numeric(tail(strsplit(x,'[.]')[[1]],-1L))))
}


#' take a named or numbered side and give whether it is an x or y axis
#'
#' @param side numeric or named \code{side}
#' @return "x" or "y" character vector
#' @keywords internal
as.axis <- function(x) UseMethod("as.axis")

#' @keywords internal
#' @export
as.axis.character <- function(side) {
  side <- as.side(side)
  axis <- as.axis(side)
  return(axis)
}

#' @keywords internal
#' @export
as.axis.numeric <- function(side) {
  axis <- rep("x", length(side))
  is.y <- side %% 2 == 0
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
  if(any(to.set)) class(side$lim) <- class(lims) 
  side$lim[to.set] <- lims[to.set]
  side$usr.lim[to.set] <- TRUE
  return(side)
}



#' set the axes logical on a side
#' 
#' @param args arguments to pull log info from
#' @param side the side for the gsplot object (see \code{\link{sides}})
#' @return a modified \code{side}
#' @keywords internal
set_side_axes <- function(args, side, side.num){
  stopifnot(length(side.num) == 1)
  
  side$axes <- ifelse(exists("axes", args), args$axes, side$axes)
  return(side)
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
    lab <- args[[lab.arg]]
    side$label <- ifelse(is.expression(lab) || !is.na(lab), lab, side$label)
  }
  return(side)
}

#' sets the side limits according to a new addition
#' 
#' @param args pass through args from graphics call
#' @param side the side for the gsplot object (see \code{\link{as.side}})
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

  free.lim <- !side$usr.lim
  if(any(free.lim)){
    side.vals <- strip_pts(args, include)
    if (any(!is.na(side.vals))){
      data.range <- range(c(side.vals[is.finite(side.vals)], side$lim), na.rm = TRUE)
      data.range[!free.lim] <- side$lim[!free.lim]
      side$lim <- data.range
    }
  }
  
  return(side)
}

#' add a new default side to gsplot
#' 
#' @param object a gsplot object
#' @param side.name a character for side name, in the form of 'side.1'
#' @return a modifies gsplot object
add_new_side <- function(object, side.name){
  stopifnot(length(side.name) == 1)
  if (side.name %in% side_names(object))
    stop(side.name, ' already exists, cannot add it.', call. = FALSE)
  side.template <- list(list(
    axis = set_args('axis', side=as.side(side.name), package='graphics'), 
    lim = c(NA, NA), log=FALSE, label="", axes = TRUE, reverse = FALSE, usr.lim=c(FALSE, FALSE)))
  
  names(side.template) <- side.name
  
  last.side.i <- max(which_sides(object), 0)
  object <- append(object, side.template, after = last.side.i)
  object <- modify_side_par(object, arguments=list(c()), side=as.side(side.name))
  return(object)
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

#' return sides that are either x (odd)
#' 
#' @param sides an integer vector of any length 
#' @return the side that corresponds to x
#' @keywords internal
get_sides_x <- function(sides){
    sides[sides %% 2 != 0]
}

#' return sides that are either y (even)
#' 
#' @param sides an integer vector of any length 
#' @return the side that corresponds to y
#' @keywords internal
get_sides_y <- function(sides){
  sides[sides %% 2 == 0]
}

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

#' turn a numeric side into a named side
#' 
#' @param sides an integer vector of \code{side}
#' @return a character vector of side names
#' @keywords internal
as.side_name <- function(sides){
  paste('side.',sides, sep='')
}

#' take a named side and turn it into numeric
#' 
#' @param side.names vector of side names
#' @return numeric values for \code{side}
#' @keywords internal
as.side <- function(side.names){
  as.numeric(gsub('side.','',side.names))
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
#' @return a sides list
#' @keywords internal
sides <- function(gsplot){
  gsplot[which_sides(gsplot)]
}

#' sets the user-defined limits to sides
#' 
#' @param lims the usr.lims
#' @param sides a sides list
#' @return the modified sides list
#' @keywords internal
set_usr_lim <- function(lims, sides){
  for (lim in names(lims)){
    to.set <- !is.na(lims[[lim]])
    sides[[lim]]$lim[to.set] <- lims[[lim]][to.set]
    sides[[lim]]$usr.lim[to.set] <- TRUE
  }
  return(sides)
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
#' @param view a single named view
#' @param sides the sides for the gsplot object (see \code{\link{sides}})
#' @return a modified \code{sides} list
#' @keywords internal
set_side_log <- function(view, sides){
  stopifnot(length(view) == 1)
  view.name <- names(view)
  log <- summarize_args(view, c('log'), ignore=c('gs.config'), na.value = "")[[view.name]]
  if (log == "")
    return(sides) # // do nothing
  
  x.side.name <- as.x_side_name(view.name)
  y.side.name <- as.y_side_name(view.name)
  sides[[x.side.name]]$log <- ifelse(grepl('x',log), TRUE, sides[[x.side.name]]$log) # replace only if specified
  sides[[y.side.name]]$log <- ifelse(grepl('y',log), TRUE, sides[[y.side.name]]$log) 
  return(sides)
}

#' set the label value on a side
#' 
#' @param view a single named view
#' @param sides the sides for the gsplot object (see \code{\link{sides}})
#' @return a modified \code{sides} list
#' @keywords internal
set_side_lab <- function(view, sides){
  stopifnot(length(view) == 1)
  view.name <- names(view)
  
  x.side.name <- as.x_side_name(view.name)
  y.side.name <- as.y_side_name(view.name)
  ylab <- summarize_args(view, c('ylab'), ignore=c('gs.config'), na.value = NA)[[view.name]]
  xlab <- summarize_args(view, c('xlab'), ignore=c('gs.config'), na.value = NA)[[view.name]]
  sides[[x.side.name]]$label <- ifelse(!is.na(xlab), xlab, sides[[x.side.name]]$label)
  sides[[y.side.name]]$label <- ifelse(!is.na(ylab), ylab, sides[[y.side.name]]$label)
  return(sides)
}

#' sets the side limits according to a new view addition
#' 
#' @param view a single view
#' @param sides the sides for the gsplot object (see \code{\link{sides}})
#' @return a modified \code{sides} list
#' @keywords internal
set_side_lim <- function(view, sides){
  y.include <- c('y','y1','y0','ytop','ybottom')
  x.include <- c('x','x1','x0','xleft','xright')
  # // need value arguments, need yaxs/xaxs args, need user-specified ylim/xlim values
  usr.lims <- c(summarize_side_values(view, 'ylim', ignore=c('window','gs.config'), axis='y'),
                summarize_side_values(view, 'xlim', ignore=c('window','gs.config'), axis='x'))
  sides <- set_usr_lim(usr.lims, sides)
  
  locked.sides <- locked_sides(sides)
  side.vals <- c(summarize_side_values(view, y.include, ignore=c('window','gs.config'), axis='y', skip.side = locked.sides),
                 summarize_side_values(view, x.include, ignore=c('window','gs.config'), axis='x', skip.side = locked.sides))
  
  for (side in names(side.vals)){
    data.vals <- side.vals[[side]]
    if (any(!is.na(data.vals))){
      data.range <- range(c(data.vals[is.finite(data.vals)], sides[[side]]$lim), na.rm = TRUE)
      free.lim <- !sides[[side]]$usr.lim
      data.range[!free.lim] <- sides[[side]]$lim[!free.lim]
      sides[[side]]$lim <- data.range
    }
  }
  return(sides)
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
  
  side <- as.side_name(strsplit(view_nm, '[.]')[[1]][1 + side_i[[axis]]])
  
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
    side_list <- rep(list(side_template), sum(to_add)) %>% setNames(sides[which(to_add)])
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
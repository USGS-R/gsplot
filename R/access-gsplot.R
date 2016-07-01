
#' xlab for gsplot
#' 
#' get the xlab for views in gsplot object
#' 
#' @param object a gsplot object
#' @param side which side(s) to use
#' 
#' @export
xlab <- function(object, side) UseMethod("xlab")

#' @export
xlab.gsplot <- function(object, side=NULL){
  label(object, side, 'x')
}

#' ylab for gsplot
#' 
#' get the ylab for views in gsplot object
#' 
#' @param object a gsplot object
#' @param side which side(s) to use
#' 
#' @export
ylab <- function(object, side) UseMethod("ylab")

#' @export
ylab.gsplot <- function(object, side=NULL){
  label(object, side, 'y')
}

label <- function(object, side, axis){
  side.names <- names(sides(object))
  if (!is.null(side))
    side.names <- as.side_name(side)
  else {
    if (!is.null(axis)){
      sides <- as.side(names(sides(object)))
      if (axis == 'y')
        use.sides <- sides %% 2 == 0
      else 
        use.sides <- !sides %% 2 == 0
      side.names <- as.side_name(sides[use.sides])
    } 
    
  }
  
  labels <- lapply(side.names, function(x) object[[x]]$label) %>% 
    setNames(side.names)
  if (!is.null(side) && length(side==1))
    labels <- labels[[1]]
  return(labels)
}

#' xlim for gsplot
#' 
#' get the xlim for views in gsplot object
#' 
#' @param object a gsplot object
#' @param side which side(s) to use
#' @param set.undefined logical, use opposite side if this one is undefined?
#' 
#' @export
xlim <- function(object, side, set.undefined) UseMethod("xlim")

#' @export
xlim.gsplot <- function(object, side=NULL, set.undefined=TRUE){
  lim(object, side, axis='x', set.undefined=set.undefined)
}

#' ylim for gsplot
#' 
#' get the ylim for views in gsplot object
#' 
#' @param object a gsplot object
#' @param side which side(s) to use
#' @param set.undefined logical, use opposite side if this one is undefined?
#' 
#' @export
ylim <- function(object, side, set.undefined) UseMethod("ylim")

#' @export
ylim.gsplot <- function(object, side=NULL, set.undefined=TRUE){
  lim(object, side, axis='y', set.undefined=set.undefined)
}

#' limits for gsplot
#' 
#' get the limits for sides in gsplot object
#' 
#' @param object a gsplot object
#' @param side which side(s) to use
#' @param axis 'y' or 'x'. Only used when side=NULL
#' @param set.undefined logical, use opposite side if this one is undefined?
#' @param if.null replace with this value when limits are NULL
#' 
#' @export
lim <- function(object, side, axis, set.undefined, if.null) UseMethod("lim")

#' @export
lim.gsplot <- function(object, side=NULL, axis = NULL, set.undefined=TRUE, if.null=c(0,1)){
  all.side.names <- names(sides(object))
  side.names <- all.side.names
  if (!is.null(side)) {
    side.names <- as.side_name(side)
    side.axis <- as.axis(side)
    side.wrong.axis <- side.names[side.axis != axis]
    if(!is.null(axis) && length(side.wrong.axis) > 0){ 
      warning(paste("no", axis, "axis limits for", paste(side.wrong.axis, collapse="+")))
    }
  } else {
    if (!is.null(axis)){
      sides <- as.side(names(sides(object)))
      if (axis == 'y')
        use.sides <- sides %% 2 == 0
      else 
        use.sides <- !sides %% 2 == 0
      side.names <- as.side_name(sides[use.sides])
    } 
    
  }
  
  lims <- lapply(all.side.names, function(x) {
    lim <- object[[x]]$lim
    if (object[[x]]$reverse){
      lim <- rev(lim)
    }
    return(lim)
  })
  names(lims) <- all.side.names
  
  sides.notexist <- side.names[which(!side.names %in% all.side.names)]
  for(s in sides.notexist){
    lims[[s]] <- c(NA,NA)
  }
  
  if(set.undefined){  
    # get names of all sides on the same axis (x or y) that are not completely NA
    which.undef <- sapply(lims, function(x) all(is.na(x)))
    which.undef.names <- names(lims)[which.undef]
    # only set undefined lims for sides that exist in the object, skip if the sides do not exist
    which.undef.names <- which.undef.names[which.undef.names %in% all.side.names]
    which.def.names <- names(lims)[!which.undef]
    if(all(which.undef)){
      lims <- NULL
    } else if(length(which.undef.names) > 0){
      undef.sides <- as.side(all.side.names[which.undef.names]) 
      def.sides <- as.side(all.side.names[which.def.names])
      if(is.null(side) || side %in% undef.sides){
        for (tmp.side in undef.sides){
          # find side closest to the undefined side (must be same axis)
          tmp.side.name <- as.side_name(tmp.side)
          tmp.lims <- lims[[tmp.side.name]]
          def.sides.axis.match <- def.sides[as.axis(def.sides) == as.axis(tmp.side)]
          closest.side <- def.sides.axis.match[which.min(abs(tmp.side-def.sides.axis.match))]
          if (length(closest.side) == 0){
            tmp.lims <- NULL
          } else {
            tmp.lims <- lims[[as.side_name(closest.side)]]  
            match.reverse <- object[[tmp.side.name]]$reverse == object[[as.side_name(closest.side)]]$reverse
            if(!match.reverse){
              warning(paste("undefined limits for side", tmp.side, 
                            ", cannot reverse; therefore, matching side", closest.side))
            }
          }
          lims[[tmp.side.name]] <- tmp.lims
        }
      }
    }
  }
  
  if (!is.null(lims) && !is.null(side) && length(side)==1){ ## move this to the end
    lims <- lims[[side.names]]
  } else {
    lims <- lims[side.names]
  }
  
  if (is.null(lims)){
    lims <- if.null
  }
  
  return(lims)
}


#' log for gsplot
#' 
#' get the log for views in gsplot object
#' 
#' @name logged
#' @param object a gsplot object
#' @param side which side(s) to use (returns logical)
#' @examples 
#' gs <- gsplot() %>%
#'    points(1, 2, legend.name="Cool points", xlim=c(0,NA)) %>%
#'    lines(x=1:5, y=1:5, legend.name="Cool lines", ylab='taco night', log='x')
#'    
#' logged(gs, 1)
#' logged(gs)
#' logged(gs, c(1,2))
#' @export
logged <- function(object, side) UseMethod('logged')

#' @rdname logged
#' @export
logged.gsplot <- function(object, side=NULL){
  
  if (!is.null(side) && length(side) == 1){
    return(sapply(side, function(x) object[[as.side_name(x)]]$log))
  } 
  
  if (is.null(side))
    side = sort(as.side(names(sides(object))))
  
  logged <- lapply(side, function(x) logged.gsplot(object, x))
  return(setNames(logged, as.side_name(side)))
}

as.log <- function(object, view.name){
  x.side <- as.x_side(view.name)
  y.side <- as.y_side(view.name)
  paste0(ifelse(logged(object, x.side),'x',''),ifelse(logged(object, y.side),'y',''))
}

#' Get view information from a gsplot object
#' 
#' get the views in gsplot object
#' 
#' @param object a gsplot object
#' @return data frame with one row per view. Each view has an x side, y side, the log command, and the view index.
#' @export
view_info <- function(object){
  j <- which_views(object)
  if (length(j) == 0){
    return(NULL)
  }
  
  viewNames <- names(object[j])
  x.sides <- as.x_side(viewNames)
  y.sides <- as.y_side(viewNames)
  viewLogs <- sapply(viewNames , function(x) as.log(object, x))
  viewInfo <- data.frame(t(rbind(x.sides, y.sides, viewLogs, j, viewNames)), stringsAsFactors = FALSE, row.names = NULL)
  
  names(viewInfo) <- c("x","y","log","index","name")
  
  for(dir in c("y","x")){
    dup_index <- which(duplicated(viewInfo[dir]) | duplicated(viewInfo[dir], fromLast=TRUE))
    viewInfo$log[dup_index[-grep(dir,viewInfo$log)]] <- paste0(dir,viewInfo$log[dup_index[-grep(dir,viewInfo$log)]])
  }
  
  viewInfo[,c("x","y","index")] <- sapply(viewInfo[,c("x","y","index")], as.integer)
  
  i <- which(names(object) %in% 'axis')
  defined.sides <- sapply(i, function(x) object[[x]][['arguments']][['side']])
  view.sides.drawn <- NULL

  viewInfo$x.side.defined.by.user <- viewInfo$x %in% defined.sides
  viewInfo$y.side.defined.by.user <- viewInfo$y %in% defined.sides
  
  return(viewInfo)
}
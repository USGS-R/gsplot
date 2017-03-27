#' gsplot axis
#'
#' Formats axes for the plotting region.  See \code{\link[graphics]{axis}} for more details.
#'
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' 
#' @details Additional graphical parameter inputs: 
#' \itemize{
#'  \item{\code{side}} {integer indicating the bottom(1), left(2), top(3), or right(4) side of the plot to draw the axis}
#'  \item{\code{labels}} {logical value indicating whether numerical values are shown at the tickmarks, or a character vector of labels for each tickmark}
#'  }
#'    
#' @rdname axis
#' @seealso \code{\link[graphics]{axis}}
#' @export
#' @examples
#' gs <- gsplot() %>%
#'    points(x=1:5, y=1:5, legend.name="Stuff") %>%
#'    lines(2:6, y=2:6, ylim=c(0,10)) %>%
#'    background_color(col="lightgoldenrod") %>%
#'    axis(side=c(3,4),labels=FALSE) %>%
#'    legend("topright")
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),las=0) %>%
#'    axis(side=c(4), labels=FALSE) %>%
#'    axis(side=c(1,3), n.minor=4)
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:10, 1:10) %>%
#'    axis(1, at = seq(0,10,by=0.1),labels=FALSE, tcl=0.15) %>%
#'    axis(2, reverse=TRUE)
#' gs
#' 
#' gs <- gsplot() %>%
#'    points(1:5, c(1,10,100,1000,10000), log="y") %>%
#'    axis(side=c(2,4), n.minor=4, las=1)
#' gs
#' 
#' gs <- gsplot() %>%
#'    lines(1:5, c(1,10,100,1000,10000), log="y", axes=FALSE) %>%
#'    axis(side=c(2,4), labels=FALSE, n.minor=4)
#' gs
#' 
#' gs <- gsplot(xaxs='r', yaxs='r') %>%
#'    lines(1:5, c(1,10,100,1000,10000), log="y", xaxt='n') %>%
#'    axis(side=c(2,4), labels=FALSE, n.minor=4)
#' gs
#' 
#' gs <- gsplot() %>% 
#'   points(runif(30, 1,5), runif(30,0.5,3.5)) %>% 
#'   axis(side=1, at=seq(1,5,by=0.25),las=3) %>%
#'   axis(side=c(3,4), labels=FALSE)
#' gs
#' 
#' usrDef <- gsplot(mar=c(4,4,4,4)) %>% 
#'   points(x=1, y=2, side=c(3,2), cex=3, xlab='cat',log='x') %>% 
#'   points(x=3:10,y=4:11, side=c(1,2), log='y')
#' usrDef
axis <- function(object, ...) {
  override("graphics", "axis", object, ...)
}

#' @param n.minor number of minor ticks between major ticks
#' @param tcl.minor tick length for minor ticks (used if \code{n.minor} > 0). Use NA (the default) to have this 
#' calculated automatically as half of the value for \code{par('tcl')} at the time of rendering the axis. 
#' @param reverse flip the orientation of the axis?
#' @rdname axis
#' @param append replace or append an existing axis for this side (logical)
axis.gsplot <- function(object, ..., n.minor=0, tcl.minor=NA, reverse=NULL, append=FALSE) {
  
  fun.name <- "axis"
  
  args <- filter_arguments(fun.name = fun.name, ..., 
                           custom.config = object[["global"]][["config"]][["config.file"]])
  
  if (length(args$extracted.args) > 0){
    warning('axis.gsplot is not configured to support embedded function calls', call. = FALSE)
  }
  
  user.args <- args$call.args
  sides <- user.args[[fun.name]]$side
  
  if (is.null(sides)){
    return(object)
  }
  
  user.args[[fun.name]]$side <- NULL
  user.args[[fun.name]]$n.minor <- n.minor
  user.args[[fun.name]]$tcl.minor <- tcl.minor
  
  user.args[[fun.name]] <- append_replace(user.args[[fun.name]], args$option.args)
  
  for(side in sides){
    # append the side and give it defaults if it doesn't exist
    if(!append){
      object[[paste0("side.",side)]] <- NULL
      object <- modify_side(object, args = list(), side=side) 
    } else {
      side.name <- paste0("side.",side)
      user.args$axis <- c(side = side, user.args$axis)
      object[[side.name]] <- c(object[[side.name]],user.args)
    }
    
    object[[as.side_name(side)]][['usr.axes']] <- TRUE
    object[[as.side_name(side)]][['axis']] <- append_replace(object[[as.side_name(side)]][['axis']], user.args[[fun.name]])
    if (!is.null(reverse)){
      object[[as.side_name(side)]][['reverse']] <- reverse
    }
  }
  
  class(object) <- 'gsplot'
  return(object)
  
}

draw_axis <- function(object, side.name){
  
  
  # method isn't made for multiple axis calls
  which.axis <- which(names(object[[side.name]]) == 'axis')
  if (length(which.axis) > 1){
    for (axis.i in which.axis){
      tmp <- object
      tmp[[side.name]] <- tmp[[side.name]][-which.axis[which.axis %in% axis.i]]
      draw_axis(tmp, side.name)
    }
  }
  
  axis.args <- object[[side.name]][['axis']]
  side.lim <- object[[side.name]][['lim']]
  
  axis.args$at <- get_axTicks(object, as.side(side.name))
  
  # need a cleaner way to extract the non-axis args (such as n.minor and tcl.minor)
  
  if(!exists('n.minor',axis.args) || axis.args$n.minor == 0){
    axis.args$n.minor <- NULL
    axis.args$tcl.minor <- NULL
    do.call('Axis', axis.args)
  } else {
    n.minor <- axis.args$n.minor + 1
    
    tcl <- NULL
    if (exists('tcl.minor',axis.args)){
      tcl <- ifelse(is.na(axis.args$tcl.minor), par('tcl')*0.5, axis.args$tcl.minor)
    }
    
    axis.args$n.minor <- NULL
    axis.args$tcl.minor <- NULL
    
    do.call('Axis', axis.args)
    
    
    
    # Minor axis:
    
    #if user hasn't specified "at", calculate it
    if(is.null(axis.args$at)){
      at <- axTicks(axis.args$side)
    } else {
      at <- axis.args$at
    }
    
    newAT <- c()
    
    if(axis.args$side %% 2 == 0){
      logScale <- par()$ylog
    } else {
      logScale <- par()$xlog
    }
    
    if(logScale){
      spacing <- median(diff(log10(at)))
      at <- c(10^(log10(at[1])-spacing),at,10^(log10(at[length(at)])+spacing))
    } else {
      spacing <- median(diff(at))
      at <- c(at[1]-spacing,at,at[length(at)]+spacing)
    }
    
    for(i in 2:length(at)){
      newAT <- c(newAT, seq(at[i-1], length.out = n.minor, by= (at[i]-at[i-1])/n.minor))
    }
    
    axis.args$at <- newAT
    axis.args$labels <- FALSE
    axis.args$tcl <- tcl
    do.call('Axis', axis.args)
  }
}

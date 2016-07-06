
#' set lim for gsplot
#' 
#' set the lim for views in gsplot object
#' 
#' @param object a gsplot object
#' @param value the new limits to use (vector)
#' @param side which side(s) to use
#' 
#' @examples 
#' myplot <- gsplot() %>% 
#'   points(1:3, 2:4) %>%
#'   lines(1:5, 5:1)
#' lim(myplot, side=1)<-c(1,4)
#' myplot
#' @export
`lim<-` <- function(object, value, side) UseMethod('lim<-', object)

#' @export
`lim<-.gsplot` <- function(object, value, side=NULL){
  if (is.null(side)){
    # // value must be a list
    stop('list value for `lim<-.gsplot` is not implemented')
  } else {
    # // side needs to be length 1, unless we deal w/ it
    # // deal w/ setting reverse back to FALSE?
    # // hack to deal w/ lim(myplot, side=1)[1] <- 0.25 case w usr.lim setting
    set.i <- !is.na(value) & lim(object, side=side) != value
    object[[as.side_name(side)]]$lim[set.i] <- value[set.i]
    object[[as.side_name(side)]]$usr.lim <- set.i
    
  }
  return(object)
}
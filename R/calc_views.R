#' @export
calc_views <- function(gsplot){
  
  sides <- data.frame(x=c(),y=c())
  for (i in seq_len(length(gsplot))){
    draw_sides <- gsplot[[i]]$side
    if (!is.null(draw_sides)){
      sides <- rbind(sides, data.frame(x=gsplot[[i]]$side[1], y=gsplot[[i]]$side[2]))
    }
  }
  
  unique_views <- unique(sides)
  
  return(unique_views)
}
#' @export
calc_views <- function(gsplot){
  
  
  unique_sides <- unique(lapply(gsplot, function(x) x$side))
  views <- rep(list(view=c()),length(unique_sides))
  
  for (i in 1:length(unique_sides)){
    views[[i]]$side = unique_sides[[i]]
  }
  
  for (i in seq_len(length(gsplot))){
    draw_sides <- gsplot[[i]]$side
    if (!is.null(draw_sides)){
      view_i <- which(sapply(unique_sides, function(x) x[1] == draw_sides[1] & x[2] == draw_sides[2]))
    } else {
      view_i = 1 
    }
    to_draw <- setNames(list(gsplot[[i]][ names(gsplot[[i]]) != "side"]), names(gsplot[i]))
    views[[view_i]] <- append(views[[view_i]], to_draw)
  }
  
  
  return(views)
}

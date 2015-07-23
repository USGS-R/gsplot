set_view_order <- function(views){
  #Start enforcing some order. Background color has to go in back...I'd also put grid in back:
  if("legend" %in% names(views)){
    legendChunk <- views$legend
    views <- views[-which(names(views) %in% "legend")]
  }
  
  
  if("grid" %in% names(do.call(c, unname(views)))){
    newView <- list()
    for(i in 1:length(views)){
      subView <- views[[i]]
      if("grid" %in% names(subView)){
        gridView <- subView$grid
        otherViews <- subView[-which(names(subView) %in% "grid")]
        subView <- append(list(grid=gridView), otherViews) 
      }
      newView <- append(newView, list(view=subView))
    }
    views <- newView
    
  }
  
  if("bgCol" %in% names(do.call(c, unname(views)))){
    newView <- list()
    for(i in 1:length(views)){
      subView <- views[[i]]
      if("bgCol" %in% names(subView)){
        bgColView <- subView$bgCol
        otherViews <- subView[-which(names(subView) %in% "bgCol")]
        subView <- append(list(bgCol=bgColView), otherViews)
      }
      newView <- append(newView, list(view=subView))
    }
    views <- newView
  }
  
  if("legend" %in% names(views)){
    views <- append(views, list(legend=legendChunk))
  }
  return(views)
}
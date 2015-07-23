set_view_order <- function(views, order){
  #Start enforcing some order. Background color has to go in back...I'd also put grid in back:
  
  dataViews <- views[-which(names(views) %in% c("par","legend"))]
  otherViews <- views[which(names(views) %in% c("par","legend"))]
  
  for(specialCall in order){
    if(specialCall %in% names(do.call(c, unname(views)))){
      newView <- list()
      for(i in 1:length(dataViews)){
        subView <- dataViews[[i]]
        if(specialCall %in% names(subView)){
          specialView <- subView[specialCall]
          otherViews <- subView[-which(names(subView) %in% specialCall)]
          subView <- append(assign(specialCall,specialView), otherViews) 
        }
        newView <- append(newView, list(view=subView))
      }
      dataViews <- newView
      
    }
  }

  views <- append(dataViews, otherViews)

  
  return(views)
}
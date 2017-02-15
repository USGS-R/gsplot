#' gsplot
#'
#' Used to change the class of inputs to "gsplot".
#'
#' @param x list
#' @param created vector of length one giving the date the gsplot object was created. Defaults to 
#' using \code{Sys.Date()}. Output class matches that of the input.
#' @param gsplot.version vector of length one giving the version of the gsplot package used to create the
#' object. Defaults to calling \code{packageDescription()}. Output class matches that of the input.
#' @param config.file path to the file that will only be used for setting 
#' par in this one gsplot object. If \code{NA} (default), par is set by the global options set by
#' loadConfig().
#' @param theme path to the file that will only be used for setting 
#' the gsplot theme in this one gsplot object. If \code{NA} (default), there is no theme.
#' @param frame.plot a logical indicating whether a box should be drawn around the plot. Default is \code{TRUE}.
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' @return gsplot 
#' @export
#' @rdname gsplot
#' @importFrom methods getPackageName
#' @importFrom utils packageDescription
#' @examples
#' gsplot() 
#' gsplot(theme = theme.hadley)
#' gs_config <- gsplot(config.file = 
#' system.file("extdata", "lineScatter.yaml", package = "gsplot")) %>%
#' lines(1:10, 1:10)
#' 
#' gs_config
#' 
#' gs <- gsplot(theme = theme.hadley) %>%
#'         points(1:10, 1:10, xlab="Index")
#' gs
#' 
gsplot <- function(x = NULL, ...) UseMethod("gsplot")

#' @rdname gsplot
#' @export
gsplot.default <- function(..., created=Sys.Date(), 
                           gsplot.version=packageDescription(getPackageName(), 
                                                             fields = "Version"),
                           config.file=NA, theme=NA, frame.plot=TRUE) {
  
  user.config <- config.file
  
  if(!all(is.na(theme))){
    object <- theme
    
    if("metadata" %in% names(object)){
      object <- object[-which(names(object) == "metadata")]
    }
    
    if(is.na(config.file) && 
       "config.file" %in% names(theme[["global"]][["config"]])){ #if no config file specified by user
      config.file <- theme$global$config$config.path
    }
    
    object[["global"]][["config"]][["config.file"]] <- !is.na(config.file)
    object[["global"]][["config"]][["config.path"]] <- config.file
    
  } else {
    object <- list(global= list(config=list(frame.plot=frame.plot,
                                            config.file = !is.na(config.file),
                                            config.path = config.file)))
  }
  
  object <- c(list(metadata = list(created=created,
                               gsplot.version=gsplot.version)),
                 object)

  if (!is.na(user.config)){
    object[["config"]] <- yaml.load_file(config.file)
  } 
  
  if(object[["global"]][["config"]]$config.file){
    load_temp_config(object)
  }

  if(length(all.equal(gsconfig$original.par, par(no.readonly = TRUE))) > 1){
    par(gsconfig$original.par)
  }
  
  if(!("par" %in% names(object[["global"]]))){
    object <- add_new_par(object, 'global')
  } 
   
  object <- gsplot(object)

  if(length(list(...)) > 0){
    object <- par(object, ...)
  }

  return(object)
}

#' @rdname gsplot
#' @exportMethod gsplot
gsplot.list <- function(x){
  
  class(x) <- "gsplot"
  invisible(x) 
}

#' Summary of gsplot object
#'
#' Summary information
#'
#' @param object list
#' @param \dots additional parameters
#' @export
#' @examples 
#' gs <- gsplot() %>%
#'        points(1:10,1:10) %>%
#'        axis(side=1, at=seq(1,10,length.out=18),las=3) %>%
#'        axis(side=3, labels=FALSE) %>%
#'        grid(side=c(1,2),col="green") %>%
#'        grid(side=c(3,4))
#' summary(gs)
summary.gsplot <- function(object,...){
  
  view.info <- view_info(object)
  cat("Summary information of plotting object:\n")
  cat(nrow(view.info),"views:\n")
  for(i in seq_len(nrow(view.info))){
    cat("View:",i,"\nx side:", view.info$x[i], ",y side:", view.info$y[i], "\n")
    cat("xlim:",xlim(object, side=view.info$x[i]),",")
    cat("ylim:",ylim(object, side=view.info$y[i]))
    if(view.info$log[i] != ""){
      cat(",log:",view.info$log[i])
    }
    cat("\n")
  }
}


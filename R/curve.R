#' gsplot curve
#'
#' Plot a mathematical function of x on a new or existing plotting region. 
#'
#' @param object gsplot object
#' @param \dots Further graphical parameters may also be supplied as arguments. See 'Details'.
#' 
#' @details Additional graphical parameter inputs: 
#' \itemize{
#'  \item{\code{expr}} {mathematic function in terms of x}
#'  \item{\code{from}} {x value indicating the beginning of the plotted function}
#'  \item{\code{to}} {x value indicating the end of the plotted function}
#'  \item{\code{n}} {integer, number of x values at which to evaluate {\code{expr}}}
#'  \item{\code{add}} {logical, TRUE=add to existing plot, FALSE=create new plot (default)}
#'  \item{\code{xlim}} {vector indicating the lower and upper x values of the plotting window}
#'  \item{\code{ylim}} {vector indicating the lower and upper y values of the plotting window}
#'  \item{\code{type}} {plot type, where 'l'=line, 'p'=point, etc}
#'  \item{\code{log}} {string indicating axes to have a log scale, where ""=none, "x"=x-axis, "y"=y-axis, and "xy"=both axes}
#'  \item{\code{col}} {line color}
#'  \item{\code{lty}} {line type}
#'  \item{\code{lwd}} {line width}
#'  }
#'    
#' @rdname curve
#' @export
#' @examples
#' gs <- gsplot() %>%
#'    points(x=c(1:5, 3.5), y=c(1:5, 6), legend.name="Stuff") %>%
#'    lines(x=2:6, y=2:6, ylim=c(0,10)) %>%
#'    axis(side=c(1,2),labels=TRUE) %>%
#'    legend("topright")
#'    curve(x^3, col=blue, add=TRUE)
#' gs
#' 
#' gs <- gsplot() %>%
#'    lines(x=10:20, y=c(10:15, 25, 17:20), 
#'          xlim=c(0,30), ylim=c(0,30), col='darkgreen', 
#'          legend.name="Some data") %>%
#'    legend()
#'    curve(sin(x), from=0, to= ) %>%
#' gs
curve <- function(object, ...) {
  override("graphics", "curve", object, ...)
}

curve.gsplot <- function(object, ..., xname="x", legend.name=NULL, side=c(1,2)){
  
  current_list <- config("curve")
  

  sexpr <- substitute(...)
  if (is.name(sexpr)) {
    expr <- call(as.character(sexpr), as.name(xname))
  }
  else {
    if (!((is.call(sexpr) || is.expression(sexpr)) && xname %in% 
          all.vars(sexpr))) 
      stop(gettextf("'expr' must be a function, or a call or an expression containing '%s'", 
                    xname), domain = NA)
    expr <- sexpr
  }
  x <- seq.int(from, to, length.out = n)
  ll <- list(x = x)
  names(ll) <- xname
  y <- eval(expr, envir = ll, enclos = parent.frame())
  lines(x = x, y = y, type = type, ...)
  
  arguments <- list(...)

  indicesToAdd <- !(names(current_list) %in% names(arguments))
  arguments <- append(arguments, current_list[indicesToAdd])
  
  object <- append(object,  list(curve = list(arguments = arguments, 
                                              gs.config=list(legend.name = legend.name, 
                                                             side = side))))
  
  return(gsplot(object))
  
}

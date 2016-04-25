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
#'  \item{\code{legend.name}} {name that appears in the legend, see \code{\link{legend}} for more legend parameters}
#'  }
#'    
#' @rdname curve
#' @export
#' @importFrom lazyeval lazy lazy_eval
#' @examples
#' gs <- gsplot() %>%
#'    points(x=c(1:5, 3.5), y=c(1:5, 6), legend.name="Stuff") %>%
#'    lines(x=2:6, y=2:6, ylim=c(0,10)) %>%
#'    axis(side=c(1,2),labels=TRUE) %>%
#'    curve(x^3, from=0, to=10, col="blue", legend.name="x^3") %>%
#'    legend("topright")
#' gs
#' 
#' gs <- gsplot() %>%
#'    curve(sin(x), from=-2*pi, to=2*pi, legend.name="sin(x)") %>%
#'    legend()
#' gs
curve <- function(object, ...) {
  override("graphics", "curve", object, ...)
}

curve.gsplot <- function(object, expr, from=0, to=1, n=101, ..., legend.name=NULL, side=c(1,2)){
  
  expr <- lazy(expr)
  
  x <- seq(from, to, length.out=n)
  y <- lazy_eval(expr, data.frame(x=x))
  object <- lines(object, x=x, y=y, ..., legend.name=legend.name, side=side)
  return(object)

  return(object)
}
#' Built-in themes 
#'  
#'\itemize{
#'  \item{theme.hadley}{Used to look like a default ggplot2 graph}
#'  \item{theme.packers}{Get a Green Bay Packers football theme}
#'}
#'
#'@aliases theme.hadley theme.packers
#'@export theme.hadley theme.packers
#'@name Themes
#'@keywords datasets
#'@examples
#' ggplotTheme <- theme.hadley
#' gbTheme <- theme.packers
#' 
#' # Code to get theme:
#' theme.hadley <- gsplot(frame.plot=FALSE) %>%
#'   bgCol(col="grey90") %>%
#'   grid(col = "white", lty = 1) %>%
#'   par(tcl=-0.2, cex=0.75) %>%
#'   axis(side = c(1,2), lwd = 0, lwd.ticks = 1)
#'   
#' theme.packers <- gsplot() %>% #TODO: add config so that points are brown
#'   bgCol(col="#203731") %>%
#'   grid(col = "gold", lty = 1, lwd=3) %>%
#'   par(tcl=-0.2, cex=0.75) %>%
#'   axis(side = c(1,2), lwd = 0, lwd.ticks = 1)
#'   
#' \dontrun{
#' save(theme.hadley, theme.packers, file="sysdata.rda")
#' }
NULL


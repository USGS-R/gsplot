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
#' ggplot_config <- system.file("extdata", "ggplot.yaml", package = "gsplot")
#' 
#' theme.hadley <- gsplot(frame.plot=FALSE, config.file=ggplot_config) %>%
#'   background_color(col="grey90") %>%
#'   grid(col = "white", lty = 1) %>%
#'   axis(side = c(1,2), lwd = 0, lwd.ticks = 1) 
#'   
#' packers_config <- system.file("extdata", "packers.yaml", package = "gsplot")
#'
#' theme.packers <- gsplot(frame.plot=FALSE, config.file=packers_config) %>% 
#'   background_color(col="#203731") %>%
#'   grid(col = "gold", lty = 1, lwd=3) %>%
#'   axis(side = c(1,2), lwd = 0)
#'   
#' \dontrun{
#' save(theme.hadley, theme.packers, file="sysdata.rda")
#' }
NULL


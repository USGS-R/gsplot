pkg.env <- new.env()
.onAttach <- function(libname, pkgname) {
  packageStartupMessage("This information is preliminary or provisional and is subject to revision. It is being provided to meet the need for timely best science. The information has not received final approval by the U.S. Geological Survey (USGS) and is provided on the condition that neither the USGS nor the U.S. Government shall be held liable for any damages resulting from the authorized or unauthorized use of the information. Although this software program has been used by the USGS, no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.")
  pkg.env$fun.details <- list(
    'points' =
      list(package='graphics', def.funs=c(graphics::plot.xy, graphics::points.default)),
    'lines' = 
      list(package='graphics', def.funs=c(graphics::plot.xy, graphics::lines.default)),
    'text' = 
      list(package='graphics', def.funs=graphics::text.default),
    'plot' = 
      list(package='graphics', def.funs=graphics::plot.xy),
    'background_color' =
      list(package='gsplot', def.funs=gsplot::background_color.default),
    'callouts' =
      list(package='gsplot', def.funs=gsplot::callouts.default),
    'error_bar' =
      list(package='gsplot', def.funs=c(gsplot::error_bar.default, graphics::plot.xy)),
    'axis' = 
      list(package='graphics', def.funs=c(graphics::axis, graphics::axis.Date, graphics::axis.POSIXct)),
    
    "par" = c(),"abline" = c(), "legend" = c(), 
    "title" = c(), "mtext" = c(), "grid" = c(), #"box" = c(),
    "segments" = c(), "arrows" = c(), "rect" = c(), 
    "polygon" = c(), "symbols" = c(), "curve" = c()
  )
  
}


#' gsplot
#'
#' \tabular{ll}{
#' Package: \tab gsplot\cr
#' Type: \tab Package\cr
#' License: \tab Unlimited for this package, dependencies have more restrictive licensing.\cr
#' Copyright: \tab This software is in the public domain because it contains materials
#' that originally came from the United States Geological Survey, an agency of
#' the United States Department of Interior. For more information, see the
#' official USGS copyright policy at
#' https://www.usgs.gov/visual-id/credit_usgs.html#copyright\cr
#' LazyLoad: \tab yes\cr
#' }
#'
#' Collection of functions to do USGS graphics.
#'
#' @name gsplot-package
#' @docType package
NULL

#' Example Daily Data
#'
#' Example data representing data from the Maumee River at Waterville OH (discharge, temperature, pH)
#'
#' @name MaumeeDV
#' @docType data
#' @examples 
#' \dontrun{
#' 
#' library(dataRetrieval)
#' 
#' sites <-c("04193490","04193500")
#' 
#' begin <- "1981-10-01"
#' end <- "2011-09-30"
#' 
#' pCodes <- c("00060","00010","00400")
#' statCd <- c("00003", "00008")
#' 
#' MaumeeDV <- readNWISdv(sites, pCodes, begin, end, statCd)
#' MaumeeDV <- renameNWISColumns(MaumeeDV)
#' }
#' head(MaumeeDV)
NULL


Installation
------------

Currently only available via github. Easiest way to install is to use the `devtools` package:

``` r
devtools::install_github("USGS-R/gsplot")
```

This package is still very much in development, so the API may change at any time.

[![Build status](https://ci.appveyor.com/api/projects/status/1nt561l271x6xhsw?svg=true)](https://ci.appveyor.com/project/jread-usgs/gsplot)

[![Build Status](https://travis-ci.org/USGS-R/gsplot.svg)](https://travis-ci.org/USGS-R/gsplot)

Overview
--------

The goal of this package is to simplify plotting in R. This includes improving the basic workflow and using defaults that tend to adhear to USGS style guidelines.

Improved workflow examples
--------------------------

``` r
library(gsplot)

demoPlot <- gsplot() %>%
   points( y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),
            col="blue", pch=18, legend.name="Points", xlab="Index") %>%
  lines(c(3,4,3), c(2,4,6), legend.name="Lines", ylab="Data") %>%
  abline(b=1, a=0, legend.name="1:1") %>%
  legend("topleft",title="Awesome!") %>%
  grid() %>%
  error_bar_vertical(x=1:3, y=c(3,1,2), y.high=c(0.5,0.25,1), y.low=0.1) %>%
  error_bar_horizontal(x=1:3, y=c(3,1,2), x.low=.2, x.high=.2, col="red",lwd=3) %>%
  arrows(x0=0.75, y0=2, x1=1, y1=2.8, lwd=2) %>%
  title("Graphing Fun") %>%
  text(.75,1.75,labels="Weird data")
demoPlot
```

![](README_files/figure-markdown_github/unnamed-chunk-2-1.png)

Disclaimer
----------

This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

[![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)](http://creativecommons.org/publicdomain/zero/1.0/)

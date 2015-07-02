# gsplot

[![Build status](https://ci.appveyor.com/api/projects/status/1nt561l271x6xhsw?svg=true)](https://ci.appveyor.com/project/jread-usgs/gsplot)  

[![Build Status](https://travis-ci.org/USGS-R/gsplot.svg)](https://travis-ci.org/USGS-R/gsplot)

##Installation
Currently only available via github. Easiest way to install is to use the `devtools` package:

```
devtools::install_github("USGS-R/gsplot")
```

This package is still very much in development, so the API may change at any time.

##Overview
The goal of this package is to simplify plotting in R. This includes improving the basic workflow and using defaults that tend to adhear to USGS style guidelines.

##Improved workflow examples

```r
library(gsplot)

demoPlot <- gsplot() %>% 
 points(x=1, y=2, legend.name="Points 1", cex=3, pch=19) %>% 
 points(x=3, y=4, side=c(1,4), legend.name="Points 2", pch=5, col="blue") %>% 
 lines(x=c(3,4,3), y=c(2,4,6), legend.name="Lines 1", lty=5, lwd=3) %>%
 lines(x=c(1,2,5), y=c(1,8,5), legend.name="Lines 2", lty=5, col="green") %>% 
 abline(a=0, b=1) %>%
 legend(location="topleft", title="Fake data")

png(filename="test.png")
demoPlot
dev.off()

```
![Result image](test.png)

##Disclaimer
This software is in the public domain because it contains materials that originally came from the U.S. Geological Survey, an agency of the United States Department of Interior. For more information, see the [official USGS copyright policy](http://www.usgs.gov/visual-id/credit_usgs.html#copyright/ "official USGS copyright policy")

Although this software program has been used by the U.S. Geological Survey (USGS), no warranty, expressed or implied, is made by the USGS or the U.S. Government as to the accuracy and functioning of the program and related program material nor shall the fact of distribution constitute any such warranty, and no responsibility is assumed by the USGS in connection therewith.

This software is provided "AS IS."

 [
    ![CC0](http://i.creativecommons.org/p/zero/1.0/88x31.png)
  ](http://creativecommons.org/publicdomain/zero/1.0/)

# gsplot


[![Build Status](https://travis-ci.org/USGS-R/gsplot.svg)](https://travis-ci.org/USGS-R/gsplot)

```R
figure <- png.new(height, width, filename) %>%
  points(x,y) %>%
  lines(x,y) %>% 
  legend(data)
```
is the same as:

```R
figure <- png.new(height, width, filename)
figure <- points(figure,x,y)
figure <- lines(figure,x,y)
figure <- legend(figure, data)
```
  

figure is a list in R, which appends all of the plotting internals, and then interatively calls 
base R plotting methods when the show method is used (this is similar to how `ggplot2` works)

show method creates figure:
```R
figure 
```

add more points and overwrite
```R
figure <- points(figure, x,y) 
```

turn figure into pdf output (calls the same show method but to pdf output)
```R
as.pdf(figure)
```
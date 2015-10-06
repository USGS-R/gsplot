## ----message=FALSE, echo=TRUE, fig.cap="Demo workflow", fig.width=6, fig.height=6----
library(gsplot)

MaumeeDV <- MaumeeDV
plot.new()
demoPlot <- gsplot() %>%
  points(y=c(3,1,2), x=1:3, xlim=c(0,NA),ylim=c(0,NA),
         col="blue", pch=18, legend.name="Points", xlab="Index") %>%
  lines(c(3,4,3), c(2,4,6), legend.name="Lines", ylab="Data") %>%
  abline(b=1, a=0, legend.name="1:1") %>%
  legend(location="topleft",title="Awesome!") %>%
  grid() %>%
  error_bar(x=1:3, y=c(3,1,2), y.high=c(0.5,0.25,1), y.low=0.1) %>%
  error_bar(x=1:3, y=c(3,1,2), x.low=.2, x.high=.2, col="red",lwd=3) %>%
  callouts(x=1, y=2.8, lwd=2, angle=250, labels="Weird data") %>%
  title("Graphing Fun")
demoPlot


## ----echo=TRUE, message=FALSE--------------------------------------------
library(gsplot)
MaumeeDV <- MaumeeDV

sites <- unique(MaumeeDV$site_no)
dates <- sapply(sites, function(x) MaumeeDV$Date[which(MaumeeDV$site_no==x)], USE.NAMES=TRUE)
flow <- sapply(sites, function(x) MaumeeDV$Flow[which(MaumeeDV$site_no==x)], USE.NAMES=TRUE)
pH <- sapply(sites, function(x) MaumeeDV$pH_Median[which(MaumeeDV$site_no==x)], USE.NAMES=TRUE)
Wtemp <- sapply(sites, function(x) MaumeeDV$Wtemp[which(MaumeeDV$site_no==x)], USE.NAMES=TRUE)


## ----echo=TRUE, fig.cap="Fig. 1 Simple flow timeseries using `gsplot`.", fig.width=6, fig.height=6----
site <- '04193500'
demoPlot <- gsplot() %>% 
  lines(dates[[site]], flow[[site]], col="royalblue") %>%
  title(main=paste("Site", site), ylab="Flow, ft3/s") %>%
  grid()
demoPlot


## ----echo=TRUE, fig.cap="Fig. 2 Simple flow timeseries with a logged y-axis using `gsplot`.", fig.width=6, fig.height=6----
site <- '04193500'
demoPlot <- gsplot() %>% 
  lines(dates[[site]], flow[[site]], col="royalblue", log='y') %>%
  title(main=paste("Site", site), ylab="Flow, ft3/s") %>%
  grid(equilogs=FALSE)
demoPlot


## ----echo=TRUE, fig.cap="Fig. 3 (a) pH vs water temperature, (b) pH timeseries, (c) water temperature timeseries.", fig.width=6, fig.height=6----
site <- '04193490'
plot1 <- gsplot() %>% 
  points(Wtemp[[site]], pH[[site]], col="black")%>%
  title(main=paste("Site", site), xlab="Water Temperature (deg C)", ylab="pH")
plot2 <- gsplot() %>% 
  lines(dates[[site]], pH[[site]], col="seagreen")%>%
  title(main="", xlab="time", ylab="pH")
plot3 <- gsplot() %>% 
  lines(dates[[site]], Wtemp[[site]], col="orangered")%>%
  title(main="", xlab="time", ylab="Water Temperature (deg C)")

layout(matrix(c(1,2,3), byrow=TRUE, nrow=3))
plot1
plot2
plot3


## ----echo=TRUE, fig.cap="Fig. 4 Water temperature timeseries on primary y-axis with pH timeseries on secondary y-axis.", fig.width=6, fig.height=6----
site <- '04193490'
demoPlot <- gsplot(mar=c(7.1, 4.1, 4.1, 4.1)) %>% 
  lines(dates[[site]], Wtemp[[site]], col="orangered", 
        legend.name="Water Temperature", ylab='Water Temperature (deg C)') %>%
  lines(dates[[site]], pH[[site]], col="seagreen", side=4, 
        legend.name="pH", ylab='pH (pH Units)') %>%
  title(main=paste("Site", site), xlab='time') %>% 
  legend(location="below")
demoPlot


## ----echo=TRUE, fig.cap="Fig. 5 Initial plot of water temperature timeseries.", fig.width=6, fig.height=6----
# initially plot the data
site <- '04193490'
demoPlot <- gsplot() %>% 
  lines(dates[[site]], Wtemp[[site]], col="orangered") %>% 
  title(main=paste("Site", site), xlab='time', ylab='Water Temperature (deg C)')
demoPlot

## ----echo=TRUE, fig.cap="Fig. 6 Plot of water temperature timeseries with 'Missing Data' callout retroactively added.", fig.width=6, fig.height=6----
# notice the missing data from ~ 1991 through ~2011 and add a callout
demoPlot <- callouts(demoPlot, x=as.Date("2000-01-01"), y=10,labels="Missing Data")
demoPlot



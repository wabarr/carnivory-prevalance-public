##plotting options
library(tidyverse)
theme_set(theme_bw(12))
gwblue<-"#033C5A"
gwbuff<-"#AA9868"
gwsecondarygreen<-"#008364"

regression_type<-"OLS"

binmins <- seq(0, 7-binwidth, by=binwidth)
binmaxes <- seq(0+binwidth, 7, by=binwidth)

timelabel <- "Time Bin Midpoint (Ma)"

##plotting rectangles 

rects <- data.frame(xmin=binmins,
                    xmax=binmaxes,
                    ymin=-Inf, 
                    ymax=Inf, 
                    odd=rep(c(TRUE, FALSE), length.out=length(binmaxes)))

##zone rectangles for the different "zones" of time
zoneRects <- data.frame(
  xmin=c(-Inf,1.9,2.6),
  xmax=c(1.9,2.6,Inf),
  ymin=-Inf, 
  ymax=Inf,
  id=c("Post Homo erectus","Pre Homo erectus","Pre-Oldowan")
)
zoneRectfillVals<-c(gwblue, gwbuff,gwsecondarygreen)


binwidth<-0.1
source("./resources/set_global_options_plotting_bins.R")
library(tidyverse)
library(stringr)
filez <- list.files("./computed-datasets/", full=TRUE, pattern="^resids_", recursive = T)

dfs <- lapply(filez, FUN=read.table, header=T, sep=",")
df <- do.call(rbind, dfs)
rm(dfs)
df$binwidth<-100
df$siteExcluded="full dataset"
df$model <- factor(df$model, labels=c("A - REC (count of levels)", "B - REC (count of sites)", "C - REC (total modified bones)"))

zoneRects[2,2] <- Inf #set upper boundary to infinity for better plotting
zoneRects <- zoneRects[1:2,]

doPlot <- function(plotBinwidth){
  forPlot <- filter(df, siteExcluded == "full dataset", binwidth==plotBinwidth)
  allbinmids <- sort(forPlot[forPlot$model=="C - REC (total modified bones)","bin"])
  forBins <- data.frame(midpoint=allbinmids[c(TRUE,FALSE)])
  singleBinPlot <- 
    ggplot(forPlot, aes(x=bin, y=resid)) + 
    geom_hline(yintercept=0, color="red", linetype="dashed") + 
    #geom_vline(xintercept = 1.9, linetype="dashed") + 
    geom_line(color="grey50") + 
    geom_point(size=6) +
    geom_label(aes(label=bin), size=1.5, label.padding=unit(0.1, "lines")) +
    facet_wrap(~model, scales="free_y", ncol=1) + 
    labs(fill=NULL, color="analytical bin width (Kyr)", x="Bin midpoint (Ma)", y="residual") + 
    scale_x_reverse() +  
    scale_fill_manual(values=zoneRectfillVals[1:2]) + 
    theme(panel.grid.minor = element_blank(), legend.position = "none")
  #return(singleBinPlot)
  ggsave(plot = singleBinPlot, filename = "./figures/Fig3.jpg", width=8, height=6)

}
doPlot(100)


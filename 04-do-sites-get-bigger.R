binwidth <- 0.1
library(tidyverse)
source("./resources/set_global_options_plotting_bins.R")

compositedata <- left_join(
  read.table(file = "./datasets/zooarchaeology_summary_stats_100Ka_full_dataset.csv",header=T, sep=","),
  read.table(file = "./datasets/zooarchaeology_summary_stats_100Ka_full_dataset.csv",header=T, sep=",")
) %>% 
  filter(bin<2.65, bin>1.15)

nlevels_sqrt_totalmod_sqrt <- 
  ggplot(compositedata, aes(x=nlevels_sqrt, y=totalmod_sqrt)) + 
  geom_point(size=12) + 
  geom_label(aes(label=bin), size=2.5) + 
  stat_smooth(method="lm", se=F, linetype="dashed", formula = y~0+x) + 
  labs(title="A") + 
  theme_bw(15) + 
  theme(legend.position="none")

nsites_sqrt_totalmod_sqrt <- ggplot(compositedata, aes(x=nsites_sqrt, y=totalmod_sqrt)) + 
  geom_point(size=12) + 
  geom_label(aes(label=bin), size=2.5) + 
  stat_smooth(method="lm", se=F, linetype="dashed", formula = y~0+x) + 
  labs(title="B") + 
  theme_bw(15) + 
  theme(legend.position="none")

nlevels_sqrt_nsites_sqrt <- ggplot(compositedata, aes(x=nsites_sqrt, y=nlevels_sqrt)) + 
  geom_point(size=12) + 
  geom_label(aes(label=bin), size=2.5) + 
  stat_smooth(method="lm", se=F, linetype="dashed", formula = y~0+x) + 
  labs(title="C") + 
  theme_bw(15) + 
  theme(legend.position="none")

pdf("./figures/FigS3.pdf", width=10, height=5)
gridExtra::grid.arrange(nlevels_sqrt_totalmod_sqrt, nsites_sqrt_totalmod_sqrt, nlevels_sqrt_nsites_sqrt, nrow=1)
dev.off()


mods <- list(lm(totalmod_sqrt~0+nlevels_sqrt, data=compositedata), 
             lm(totalmod_sqrt~0+nsites_sqrt, data=compositedata),
             lm(nlevels_sqrt~0+nsites_sqrt, data=compositedata)
)
lapply(mods, summary)
R2s <- sapply(mods, FUN=function(mod) return(summary(mod)$r.squared))
forPlot <- data.frame(bin=compositedata$bin, 
                     `B - residual levels against sites`=residuals(mods[[3]]),
                     `A - residual modified bones against sites`=residuals(mods[[2]]),
                    `C - residual modified bones against levels`=residuals(mods[[1]]),
                     nlevels_sqrt=compositedata$nlevels_sqrt,
                     totalmod_sqrt=compositedata$totalmod_sqrt,
                     nsites_sqrt=compositedata$nsites_sqrt,
                    check.names = FALSE
)

residMods <- list(
  totalmod_sqrt_nlevels_sqrt=lm(`C - residual modified bones against levels`~bin, data=forPlot),
  totalmod_sqrt_nsites_sqrt=lm(`A - residual modified bones against sites`~bin, data=forPlot),
  nlevels_sqrt_nsites_sqrt=lm(`B - residual levels against sites`~bin, data=forPlot)
)

forResidPlot <- forPlot %>%
  pivot_longer(cols = contains("against"), values_to="resid")

zoneRects[2,2] <- Inf #set upper boundary to infinity for better plotting
zoneRects <- zoneRects[1:2,]
allbinmids <- (binmaxes + binmins)/2
binmids <- data.frame(midpoint=allbinmids[allbinmids>1.2 & allbinmids<2.65][c(TRUE, FALSE)])

ggplot(forResidPlot, aes(x=bin, y=resid)) + 
  geom_rect(zoneRects, mapping=aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=id), alpha=0.3, inherit.aes = F) + 
  geom_tile(data=binmids, mapping=aes(x=midpoint, y=0),height=Inf, width=binwidth, fill=gwblue, inherit.aes = F, alpha=0.2) + 
  geom_line( alpha=0.5) +
  geom_point(size=6) + 
  geom_label(aes(label=bin), size=1.5, label.padding=unit(0.1, "lines")) +
  geom_hline(yintercept = 0, color="red", linetype="dashed") + 
  scale_x_reverse() + 
  facet_wrap(~name, scales="free_y", ncol=1) + 
  scale_fill_manual(values=zoneRectfillVals[1:2]) + 
  labs(x="Time Bin Midpoint (Ma)", y="residual", fill="") + 
  theme_bw(14) + 
  theme(legend.position="none") + 
  guides(color="none") 

ggsave(sprintf("./figures/Fig4.jpg", binwidth*1000), width=8, height=6)


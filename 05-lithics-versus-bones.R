binwidth <- 0.1
library(tidyverse)
library(ggrepel)
library(gridExtra)
source("./resources/set_global_options_plotting_bins.R")

arch <- read.table("./datasets/zooarchaeology_summary_stats_100Ka_full_dataset.csv", header=T, sep=",") %>%
  filter(bin>1.2, bin<2.65)

paleo  <- read.table("./datasets/paleontological_summary_stats_100Ka-bins.csv", header=T, sep=",")



totalmod_mod <- lm(totalmod_sqrt~lithics_sqrt, data=arch)
#nlevels_mod <- lm(nlevels_sqrt~lithics_sqrt, data=arch)
#nsites_mod <- lm(nsites_sqrt~lithics_sqrt, data=arch)

theme_set(theme_bw(9))

totalmod_plot <- ggplot(arch, aes(y=totalmod_sqrt, x=lithics_sqrt)) + 
  geom_point(size=2) + 
  geom_text_repel(aes(label=bin), size=3) + 
  stat_smooth(method="lm", se=FALSE, alpha=0.5, size=0.5) + 
  labs(y="total modified bones (sqrt)", 
       x="total lithic count (sqrt)", 
       subtitle=bquote(Total~modified~bones~against~total~lithic~count~(r^2==0.501)))



summary(totalmod_mod)





for_totalmod_residplot <- data.frame(resids=totalmod_mod$residuals, bin=arch$bin)
#for_nsites_residplot <- data.frame(resids=totalmod_mod$residuals, bin=arch$bin)
#for_nlevels_residplot <- data.frame(resids=totalmod_mod$residuals, bin=arch$bin)
for_totalmod_residplot$shape <- for_totalmod_residplot$bin < 1.9


total_mod_resids <- 
  ggplot(for_totalmod_residplot, aes(x=bin, y=resids)) + 
  geom_hline(yintercept=0, color="red", linetype="dashed",) + 
  geom_line() +
  geom_point(aes(shape=shape, color=shape), size=3) + 
  scale_x_reverse() + 
  scale_color_manual(values=c("#f1bf57", "#b04d6c")) +
  labs(title="B", subtitle="Residual total modified bones ~ lithic count", x="Bin midpoint (Ma)") + 
  geom_text(data=filter(for_totalmod_residplot, bin %in% c(1.85, 1.55, 1.35)), aes(label=bin, y=resids+2), size=3) + 
  theme(legend.position = "none")

ggsave(filename = "./figures/FigS4.jpg", plot=totalmod_plot, height=4, width=4)

       
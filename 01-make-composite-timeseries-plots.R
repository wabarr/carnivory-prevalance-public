binwidth <- 0.1
source("./resources/set_global_options_plotting_bins.R")
library(tidyverse)


fig_dir <- sprintf("./figures/")
if(!dir.exists(fig_dir)) dir.create(fig_dir, recursive = TRUE)

data_dir <- sprintf("./computed-datasets/")
if(!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)


arch <- read.table("./datasets/zooarchaeology_summary_stats_100Ka_full_dataset.csv", header=T, sep=",")
paleo <- read.table("./datasets/paleontological_summary_stats_100Ka-bins.csv", header=T, sep=",")

compositedata <- dplyr::left_join(arch, paleo)
compositedata_long <- pivot_longer(compositedata, -bin)

variable_labels <- c(
  totalmod_sqrt="Carnivory: Total count of modified bones",
  nformation_sqrt="Sampling: Count of geological formations",
  npaleo_sqrt="Sampling: Count of paleontological assemblages",
  nsites_sqrt="Carnivory: Count of published sites with modified bone",
  richness_sqrt="Sampling: Composite paleontological species richness", 
  nlevels_sqrt="Carnivory: Count of published levels with modified bone"
  
)

compositedata_long$name <- factor(compositedata_long$name, 
                                   levels=c(
                                     "totalmod_sqrt",
                                     "nsites_sqrt",
                                     "nlevels_sqrt",
                                     "richness_sqrt",
                                     "npaleo_sqrt",
                                     "nformation_sqrt"
                                   ), ordered = T)



ggplot(filter(compositedata_long, bin>1.2, bin<3.45, name != "nformation_sqrt"), 
  aes(x=bin, y=value^2)) + 
    geom_rect(data=filter(rects, odd==T,xmin>1.1, xmax<3.6), aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), alpha=0.2, inherit.aes = FALSE) + 
    geom_rect(data=zoneRects, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=id), inherit.aes=FALSE, alpha=0.3) + 
    geom_line(color=gwblue, size=0.5) + 
    geom_point(size=3) + 
    facet_wrap(~name, ncol=1, scales="free_y", labeller = labeller(name=variable_labels)) + 
    scale_x_reverse(breaks=c(3.4,2.6,1.9,1.2) ) + 
    coord_cartesian(xlim=c(3.31, 1.29)) + 
    scale_fill_manual(values=zoneRectfillVals) + 
    theme_bw(15) + 
    theme(legend.position = "none", 
          panel.grid.minor = element_blank()) + 
    labs(x="Age (Ma)", y="value")
ggsave("./figures/Fig2.jpg",width=10, height=7)    

rm(compositedata_long,compositedata, arch, paleo)


library(ggplot2)
library(ggrepel)
source("./resources/set_global_options_plotting_bins.R")
zoo <- read.csv("./datasets/Zooarch_dataset_with_100Kabin_full_dataset.csv",header=T, sep="\t")

bins <- data.frame(binmin=binmins, binmax=binmaxes) %>%
  filter(binmin>1.19, binmax<=2.71)

zoo <- filter(zoo, Area!="Dikika")
zoo <- zoo %>% 
  group_by(bin) %>% 
  mutate(bin_plot_index=1:n())




set.seed(123428) #this one is good but a bit of difficulty on Bed II sites
#set.seed(2555549)
#standard deviations for the plot
sds <- data.frame(Area=c("Gona", "Bouri", "WT", "Kanjera", "ET","Olduvai Bed I", "Olduvai Bed II", "Konso", "Peninj"),
                  sd=c(0.1, 0.1, 0.000002, 0.18,0.3,0.3,0.3,0.05,0.0000001))
zoo <- dplyr::left_join(zoo, sds)
zoo$Area <- factor(zoo$Area, levels=c("Gona", "Bouri", "WT", "Kanjera", "ET","Olduvai Bed I", "Olduvai Bed II", "Konso", "Peninj"))
zoo$jitter <- sapply(zoo$sd, FUN=function(sd) {rnorm(1, sd=sd)})
zoo$numericArea_jittered <- as.numeric(zoo$Area) + zoo$jitter
ggplot() + 
  geom_tile(bins, mapping=aes(y=(binmin+binmax)/2, x=0), 
            height=binwidth, 
            width=Inf, 
            fill="transparent", 
            color="black") + 
  scale_x_discrete(breaks="none") + 
  geom_segment(aes(x=1:9, xend=1:9, y=2.75, yend=-Inf),size=0.15, inherit.aes=F) + 
  geom_point(data=zoo, aes(x=numericArea_jittered , y=analytical_age, size=sqrt(Count_Modified_Bones), color=Area), alpha=0.6) + 
  geom_label_repel(data=zoo, aes(x=numericArea_jittered, y=analytical_age, label=Level, fill=Area), segment.alpha=0.3, size=2.5,min.segment.length = 0.25, segment.size=0.25, box.padding = 0.25, label.padding=0.1) +
  geom_label(data=zoo, aes(x=Area, y=2.78, label=Area, fill=Area), size=2.7) + 
  geom_hline(yintercept=2.7) + 
  scale_y_reverse(breaks=binmins[binmins<2.7]) + 
  labs(y="Time (Ma)", color=NULL, x=NULL, size="total modified bones (sqrt)") + 
  theme_bw(8) + 
  theme(legend.position="bottom") + 
  guides(fill="none", color="none")
ggsave("./figures/Fig1.jpg",width=9, height=6)


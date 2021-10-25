binwidth=0.1
require(ggrepel)
require(dplyr)
require(tidyr)
require(stringr)
df <- read.table("./datasets/zooarchaeology_summary_stats_100Ka_full_dataset.csv", header=T, sep=",")
modsummaries <- read.table("./computed-datasets/model_summaries/model_summaries.csv", header=T, sep=",")
modsummaries <- 
  modsummaries[!grepl("\\+", modsummaries$model),] #remove the models with a +, since we don't look at those more complex models

modsummaries$zooarch_var <- gsub("_", "", str_extract(modsummaries$model, "^[a-z]+_"))
modsummaries$paleo_var <- gsub("~ |_sqrt", "", str_extract(modsummaries$model, "~.+$"))


modsummaries$model <- gsub(" ~ ","\\~", modsummaries$model)
modsummaries$annotation <- sprintf("%s~R^2 == %s", gsub("_sqrt","",modsummaries$model), round(modsummaries$R2,2))

dataset_description<-"full_dataset"
fig_dir <- sprintf("./figures/")
if(!dir.exists(fig_dir )) {
  dir.create(fig_dir )
}

compositedata <- dplyr::left_join(df,
                                  read.table("./datasets/paleontological_summary_stats_100Ka-bins.csv", header=T, sep=",")
) %>%
  dplyr::filter(bin>1.2,bin<2.6)
names(compositedata) <- gsub("_sqrt", "" ,names(compositedata))



forPlot <- 
  compositedata %>% 
  select(-nformation) %>%
  pivot_longer(cols=c(nlevels, totalmod, nsites), names_to = "zooarch_var", values_to = "zooarch_val") %>%
  pivot_longer(cols=c(richness, npaleo), names_to = "paleo_var", values_to = "paleo_val") %>%
  left_join(modsummaries)



ggplot(forPlot, aes(x=paleo_val, y=zooarch_val)) + 
  geom_point( size=14) + 
  #poisson_smooth(se = T, linetype="dashed", color="grey40") + 
  geom_smooth(method="lm", se=F,linetype="dashed", color="grey40") + 
  geom_label(aes(label=bin), size=3) + 
  facet_grid(zooarch_var~paleo_var, scales="free", labeller = "label_parsed") + 
  labs(x="sampling proxy (sqrt)", y="carnivory proxy (sqrt)") +
  scale_color_viridis_c(direction = -1) + 
  theme_bw(18) + 
  theme(legend.position = "none") 
ggsave(sprintf("./figures/FigS1.jpg", binwidth*1000), width=15, height=10)



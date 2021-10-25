source("./resources/set_global_options_plotting_bins.R")
source("./resources/summarizeModel.R")
binwidth <- 0.1


doAnalysis <- function(zooarch_df, dataset_description){
  require(ggrepel)
  require(dplyr)
  results_table_dir <- sprintf("./computed-datasets/model_summaries")
  if(!dir.exists(results_table_dir )) {
    dir.create(results_table_dir,recursive = T)
  }
  compositedata <- dplyr::left_join(zooarch_df,
                                    read.table("./datasets/paleontological_summary_stats_100Ka-bins.csv", header=T, sep=",")
                                    ) %>%
                   dplyr::filter(bin>1.2,bin<2.6)

    mods <- list(
      mod_totalmod_richness_sqrt = lm(totalmod_sqrt~richness_sqrt, data=compositedata),
      mod_totalmod_npaleo_sqrt = lm(totalmod_sqrt~npaleo_sqrt, data=compositedata),
      mod_totalmod_npaleo_sqrt_richness_sqrt = lm(totalmod_sqrt~ npaleo_sqrt + richness_sqrt,data=compositedata),
      mod_nsites_richness_sqrt = lm(nsites_sqrt~richness_sqrt, data=compositedata),
      mod_nsites_npaleo_sqrt = lm(nsites_sqrt~npaleo_sqrt, data=compositedata),
      mod_nsites_npaleo_sqrt_richness_sqrt = lm(nsites_sqrt~ npaleo_sqrt + richness_sqrt,data=compositedata),
      mod_nlevels_richness_sqrt = lm(nlevels_sqrt~richness_sqrt, data=compositedata),
      mod_nlevels_npaleo_sqrt = lm(nlevels_sqrt~npaleo_sqrt, data=compositedata),
      mod_nlevels_npaleo_sqrt_richness_sqrt = lm(nlevels_sqrt~ npaleo_sqrt + richness_sqrt,data=compositedata)
    )


  
 
  lm_summaries <- lapply(mods, summarizeModel)
  lm_summaries <- do.call(rbind, lm_summaries)
  write.table(lm_summaries, file = sprintf("%s/model_summaries.csv",results_table_dir), row.names = F, sep=",")
 
  #change upper limit on middle bin to Inf to plot better when not ploting oldest bin
  zoneRects[zoneRects$xmin==1.9,"xmax"] <- Inf 
  plot_residuals <- function(model, saveFig=TRUE, saveResids=TRUE, data=compositedata){
    data$resid <- residuals(model)
    ggplot(data, aes(x=bin, y=resid)) + 
      geom_point() + 
      stat_smooth(se=FALSE, size=1) + 
      geom_line() + 
      scale_x_continuous(limits=c(1.2,2.601), breaks=seq(1.2, 2.6, by=0.2)) + 
      scale_fill_manual(values=zoneRectfillVals[1:2]) + 
      theme(legend.position = "none") + 
      
      labs(x="bin midpoint (Ma)", 
           y="residual",
           title=gsub("_sqrt", "", sprintf("residuals from (%s)",gsub("\\(","",model$call[2])))
      ) 
    #figFilename <- sprintf("%s/resids_%s.jpg", fig_dir , as.character(model$call)[2])
    residsFilename <- gsub("_sqrt", "", sprintf("%s/resids_%s.csv", results_table_dir , as.character(model$call)[2]))
    if(saveFig==TRUE) ggsave(figFilename, width=5.5, height=3.5)
    if(saveResids==TRUE) write.table(
      data.frame(bin=data$bin, resid=data$resid, model=as.character(model$call)[2]),
      residsFilename, 
      sep = ",",
      row.names = FALSE
      )
  }
  plot_residuals(mods$mod_nsites_richness_sqrt, saveFig = F)
  plot_residuals(mods$mod_totalmod_richness_sqrt, saveFig = F)
  plot_residuals(mods$mod_nlevels_richness_sqrt, saveFig = F)
}


## Full dataset
df <- read.table(sprintf("./datasets/zooarchaeology_summary_stats_100Ka_full_dataset.csv"), header=T, sep=",")
doAnalysis(df, "full_dataset")


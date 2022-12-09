rm(list = ls())

metadata <- read.table("Lung2Trachea.xls",header = T)
#ARG Diversity
Alpha <- read.table("sample745_metadata_AROalpha.xls",header = T,check.names = F)
sub_Alpha <- Alpha[which(Alpha$Sample %in% metadata$Sample),]

library(ggpubr)
my_comparisons <- list(c("Lung","Trachea"))
Number_compare <- compare_means(Number~Site, data=sub_Alpha,p.adjust.method = "fdr")
Number_compare
# .y.    group1 group2          p    p.adj p.format p.signif method  
# <chr>  <chr>  <chr>       <dbl>    <dbl> <chr>    <chr>    <chr>   
#   1 Number Lung   Trachea 0.0000648 0.000065 6.5e-05  ****     Wilcoxon

#Number
tiff(filename = "Lung2Trachea.Number.tif",width = 1200,height = 1800,res=600,compression="lzw")
ggboxplot(sub_Alpha, x="Site", y="Number",  color  = "Site", add = "jitter",
          palette = c("#FC4E07","#00AFBB")) + 
  guides(fill="none")+
  labs(y="Richness",x=NULL)+
  border("grey")+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  scale_y_continuous(limits = c(0,250)) +
  stat_compare_means(comparisons = my_comparisons,label = "p.format")
dev.off()

#Shannon
Shannon_compare <- compare_means(Shannon~Site, data=sub_Alpha,p.adjust.method = "fdr")
Shannon_compare
# .y.     group1 group2        p  p.adj p.format p.signif method  
# <chr>   <chr>  <chr>     <dbl>  <dbl> <chr>    <chr>    <chr>   
#   1 Shannon Lung   Trachea 0.00183 0.0018 0.0018   **       Wilcoxon
tiff(filename = "Lung2Trachea.Shannon.tif",width = 1200,height = 1800,res=600,compression="lzw")
ggboxplot(sub_Alpha, x="Site", y="Shannon",  color  = "Site", add = "jitter",
          palette = c("#FC4E07","#00AFBB")) + 
  guides(fill="none")+
  labs(y="Shannon index",x=NULL)+
  border("grey")+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  scale_y_continuous(limits = c(0,5)) +
  stat_compare_means(comparisons = my_comparisons,label = "p.format")
dev.off()

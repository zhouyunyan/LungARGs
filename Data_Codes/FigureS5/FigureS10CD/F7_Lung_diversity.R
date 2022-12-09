rm(list = ls())

##1.metadata
library(readxl)
data <- read_xlsx("Lung_meta.xlsx",sheet = "metadata")
F7_Lung <- data[which(data$Site=="Lung" & data$Population=="F7"),]
F7_Lung$Grade <- factor(F7_Lung$Grade,levels = c("HL","SLL","MLL","SVLL"))
table(F7_Lung$Grade)
# HL  SLL  MLL SVLL 
# 51  217  218  127 
my_comparisons <- list(c("HL","SLL"),c("SLL","MLL"),c("MLL","SVLL"),
                       c("HL","MLL"),c("SLL","SVLL"),
                       c("HL","SVLL"))
label <- c("HL\n(n=51)","SLL\n(n=217)","MLL\n(n=218)","SVLL\n(n=127)")

##Alpha Diversity
Alpha <- read.csv("Species_Alpha_index_2.csv",header = T,row.names = 1, check.names = F)
sub_Alpha <- Alpha[which(rownames(Alpha) %in% F7_Lung$Sample),]

sub_Alpha$Sample <- rownames(sub_Alpha)
m <- merge(F7_Lung[,c(1,5)],sub_Alpha,by="Sample")

library(ggpubr)
#Number
Number_compare <- compare_means(Number~Grade, data=m,p.adjust.method = "fdr")
Number_compare
# # A tibble: 6 x 8
# .y.    group1 group2             p      p.adj p.format p.signif method  
# <chr>  <chr>  <chr>          <dbl>      <dbl> <chr>    <chr>    <chr>   
#   1 Number HL     SLL    0.767         0.77       0.76712  ns       Wilcoxon
# 2 Number HL     MLL    0.631         0.76       0.63061  ns       Wilcoxon
# 3 Number HL     SVLL   0.000128      0.00026    0.00013  ***      Wilcoxon
# 4 Number SLL    MLL    0.232         0.35       0.23168  ns       Wilcoxon
# 5 Number SLL    SVLL   0.00000000841 0.00000005 8.4e-09  ****     Wilcoxon
# 6 Number MLL    SVLL   0.000000753   0.0000023  7.5e-07  ****     Wilcoxon

tiff(filename = "F7_Lung.Number.tif",width = 2200,height = 2300,res=600,compression="lzw")
ggviolin(m, x="Grade", y="Number",  fill  = "Grade", add = "boxplot",
         palette = "jco") + 
  guides(fill="none")+
  border("grey")+
  labs(y="Observed Species",x=NULL)+
  scale_x_discrete("Grade",labels = label)+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  stat_compare_means(comparisons = my_comparisons,
                     label = "p.format",
                     tip.length = 0)
dev.off()

#Shannon
Shannon_compare <- compare_means(Shannon~Grade, data=m,p.adjust.method = "fdr")
Shannon_compare
# # A tibble: 6 x 8
# .y.     group1 group2            p      p.adj p.format p.signif method  
# <chr>   <chr>  <chr>         <dbl>      <dbl> <chr>    <chr>    <chr>   
#   1 Shannon HL     SLL    0.389        0.39       0.38907  ns       Wilcoxon
# 2 Shannon HL     MLL    0.000862     0.0013     0.00086  ***      Wilcoxon
# 3 Shannon HL     SVLL   0.00000281   0.0000084  2.8e-06  ****     Wilcoxon
# 4 Shannon SLL    MLL    0.0000409    0.000082   4.1e-05  ****     Wilcoxon
# 5 Shannon SLL    SVLL   0.0000000275 0.00000017 2.8e-08  ****     Wilcoxon
# 6 Shannon MLL    SVLL   0.0252       0.03       0.02515  *        Wilcoxon


tiff(filename = "F7_Lung.Shannon.tif",width = 2200,height = 2300,res=600,compression="lzw")
ggviolin(m, x="Grade", y="Shannon",  fill  = "Grade", add = "boxplot",
         palette = "jco") + 
  guides(fill="none")+
  labs(y="Shannon index",x=NULL)+
  border("grey")+
  scale_x_discrete("Grade",labels = label)+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  stat_compare_means(comparisons = my_comparisons,
                     label = "p.format",
                     tip.length = 0)
dev.off()


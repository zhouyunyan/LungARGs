rm(list = ls())

data <- read.table(file = "sample745_metadata_AROalpha.xls",header=T)

F7_Lung <- data[which(data$Population=="F7" & data$Site=="Lung"),]
F7_Lung$Grade <- factor(F7_Lung$Grade,levels = c("HL","SLL","MLL","SVLL"))
write.table(F7_Lung,file = "F7_Lung_metadata_AROalpha.xls",sep = "\t",row.names = F,quote = F)

table(F7_Lung$Grade)
# HL  SLL  MLL SVLL 
# 51  217  218  127 
my_comparisons <- list(c("HL","SLL"),c("SLL","MLL"),c("MLL","SVLL"),
                       c("HL","MLL"),c("SLL","SVLL"),
                       c("HL","SVLL"))
label <- c("HL\n(n=51)","SLL\n(n=217)","MLL\n(n=218)","SVLL\n(n=127)")

library(ggpubr)
Number_compare <- compare_means(Number ~ Grade,data = F7_Lung,p.adjust.method = "fdr")
Number_compare
# # A tibble: 6 x 8
# .y.    group1 group2             p       p.adj p.format p.signif method  
# <chr>  <chr>  <chr>          <dbl>       <dbl> <chr>    <chr>    <chr>   
#   1 Number HL     SLL    0.214         0.21        0.2139   ns       Wilcoxon
# 2 Number HL     MLL    0.00818       0.012       0.0082   **       Wilcoxon
# 3 Number HL     SVLL   0.0000000515  0.00000015  5.2e-08  ****     Wilcoxon
# 4 Number SLL    MLL    0.0339        0.041       0.0339   *        Wilcoxon
# 5 Number SLL    SVLL   0.00000000234 0.000000014 2.3e-09  ****     Wilcoxon
# 6 Number MLL    SVLL   0.0000266     0.000053    2.7e-05  ****     Wilcoxon

tiff(filename = "F7_Lung_ARO.Number.tif",width = 2200,height = 2300,res=600,compression="lzw")
ggviolin(F7_Lung, x="Grade", y="Number",  fill  = "Grade", add = "boxplot",
         palette = "jco") + 
  guides(fill="none")+
  border("grey")+
  labs(y="ARG number",x=NULL)+
  scale_x_discrete("Grade",labels = label)+
  theme(axis.title.x = element_blank())+
  stat_compare_means(comparisons = my_comparisons,
                     label = "p.format",
                     tip.length = 0)
dev.off()

#Shannon
Shannon_compare <- compare_means(Shannon~Grade, data=F7_Lung,p.adjust.method = "fdr")
Shannon_compare
# # A tibble: 6 x 8
# .y.     group1 group2        p        p.adj p.format p.signif method  
# <chr>   <chr>  <chr>     <dbl>        <dbl> <chr>    <chr>    <chr>   
#   1 Shannon HL     SLL    2.32e- 1 0.23         0.232    ns       Wilcoxon
# 2 Shannon HL     MLL    1.01e- 3 0.0012       0.001    **       Wilcoxon
# 3 Shannon HL     SVLL   4.86e- 8 0.00000015   4.9e-08  ****     Wilcoxon
# 4 Shannon SLL    MLL    1.04e- 3 0.0012       0.001    **       Wilcoxon
# 5 Shannon SLL    SVLL   1.78e-10 0.0000000011 1.8e-10  ****     Wilcoxon
# 6 Shannon MLL    SVLL   7.45e- 5 0.00015      7.4e-05  ****     Wilcoxon

tiff(filename = "F7_Lung_ARO.Shannon.tif",width = 2200,height = 2300,res=600,compression="lzw")
ggviolin(F7_Lung, x="Grade", y="Shannon",  fill  = "Grade", add = "boxplot",
         palette = "jco") + 
  guides(fill="none")+
  labs(y="Shannon index",x=NULL)+
  border("grey")+
  scale_x_discrete("Grade",labels = label)+
  theme(axis.title.x = element_blank())+
  stat_compare_means(comparisons = my_comparisons,
                     label = "p.format",
                     tip.length = 0)
dev.off()
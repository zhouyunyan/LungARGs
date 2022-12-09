rm(list = ls())

ARG <- read.table("Sample745_Alpha_index_2.xls",header = T,sep = "\t",check.names = F)
MGE <- read.table("Sample745_MGE_Alpha_index_2.xls",header = T,sep = "\t",check.names = F)

names(ARG)[2:5] <- c("ARG_Number","ARG_Shannon","ARG_Simpson","ARG_Pielou")
names(MGE)[2:5] <- c("MGE_Number","MGE_Shannon","MGE_Simpson","MGE_Pielou")

data <- merge(ARG,MGE,by="Sample")

library(psych)
corr <- corr.test(data[,c(2:5)], data[,c(6:9)],use = "pairwise",method="spearman",adjust="fdr")
r <- corr$r
#             MGE_Number MGE_Shannon MGE_Simpson MGE_Pielou
# ARG_Number   0.8626666  0.07323527 -0.20806389 -0.5119703
# ARG_Shannon  0.6725799  0.21604934 -0.05477124 -0.2832820
# ARG_Simpson  0.4867846  0.23368297  0.01472975 -0.1454034
# ARG_Pielou  -0.3556346  0.16068375  0.23049904  0.3863562
p <- corr$p.adj
#             MGE_Number  MGE_Shannon  MGE_Simpson   MGE_Pielou
# ARG_Number  5.934370e-221 5.221525e-02 1.440695e-08 2.757587e-50
# ARG_Shannon  2.459207e-98 4.095571e-09 1.443059e-01 7.416913e-15
# ARG_Simpson  5.567308e-45 2.133107e-10 6.881325e-01 8.366172e-05
# ARG_Pielou   3.318454e-23 1.396747e-05 3.432227e-10 1.988958e-27

library(ggpubr)
tiff(filename = "MGE_Richness_ARG_Richness.tif",width = 1800,height = 1800,res=600,compression="lzw")
ggscatter(data , x = "MGE_Number", y = "ARG_Number", size = 1,
          add = "reg.line", conf.int = TRUE,
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          cor.coef = TRUE, cor.method = "spearman",
          xlab = "Richness (MGEs)", ylab = "Richness (ARGs)")
dev.off()

tiff(filename = "MGE_Richness_ARG_Shannon.tif",width = 1800,height = 1800,res=600,compression="lzw")
ggscatter(data , x = "MGE_Number", y = "ARG_Shannon", size = 1,
          add = "reg.line", conf.int = TRUE, 
          #add.params = list(color = "blue", fill = "lightgray"), 
          cor.coef = TRUE, cor.method = "spearman",
          add.params = list(color = "blue", fill = "lightgray"), # Customize reg. line
          cor.coeff.args = list(method = "spearman", label.y = 4.8),
          xlab = "Richness (MGEs)", ylab = "Shannon (ARGs)")
dev.off()

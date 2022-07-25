rm(list = ls())
setwd(".\\Figure5\\Figure5CD")

ARG <- read.delim("sample745_metadata_AROalpha.xls",header = T,check.names = F)
species <- read.delim("Bacteria_Species_Alpha_index_2.csv",header = T,check.names = F,sep=",")
names(species) <- c("Sample",paste("species",names(species)[-1],sep = "_"))

F7_ARG <- ARG[which(ARG$Population=="F7" &ARG$Site=="Lung"),c(1,6:10)]
names(F7_ARG) <- c("Sample",paste("ARG",names(F7_ARG)[-1],sep = "_"))
F7_species <- species[which(species$Sample %in% F7_ARG$Sample),]

data <- merge(F7_ARG,F7_species,by="Sample")

library(psych)
corr <- corr.test(data[,c(2:6)], data[,7:10],use = "pairwise",method="spearman",adjust="fdr")
r <- corr$r
#                 species_Number species_Shannon species_Simpson species_Pielou
# ARG_Number        0.8627316      0.27156416      0.24831238     0.18692621
# ARG_Shannon       0.6853770      0.30299489      0.26730920     0.23903854
# ARG_Simpson       0.5150990      0.27143262      0.23335554     0.22733886
# ARG_Pielou       -0.3193815      0.03587787      0.01037833     0.07927449
# ARG_ARO_Abun      0.6502237      0.17387248      0.17437338     0.10844823

p <- corr$p.adj
#              species_Number species_Shannon species_Simpson species_Pielou
# ARG_Number    8.558049e-182    2.044775e-11    9.136404e-10   4.522497e-06
# ARG_Shannon    3.183755e-85    5.848212e-14    3.834127e-11   3.737843e-09
# ARG_Simpson    3.851485e-42    2.044775e-11    8.355593e-09   1.933380e-08
# ARG_Pielou     2.116967e-15    3.949521e-01    7.976128e-01   5.531412e-02
# ARG_ARO_Abun   4.035731e-74    1.871410e-05    1.871410e-05   8.469258e-03

library(ggpubr)
tiff(filename = "F7_lung_species_Shannon_ARG_Richness.tif",width = 1800,height = 1800,res=600,compression="lzw")
ggscatter(data , y = "ARG_Number", x = "species_Shannon", 
          add = "reg.line", conf.int = TRUE, size = 1,
          add.params = list(color = "blue", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman",
          ylab = "Richness (ARGs)", xlab = "Shannon index (Species)")
dev.off()

tiff(filename = "F7_lung_species_Shannon_ARG_Shannon.tif",width = 1800,height = 1800,res=600,compression="lzw")
ggscatter(data , y = "ARG_Shannon", x = "species_Shannon", 
          add = "reg.line", conf.int = TRUE, size = 1,
          add.params = list(color = "blue", fill = "lightgray"),
          cor.coef = TRUE, cor.method = "spearman",
          cor.coeff.args = list(method = "spearman", label.y = 4.8),
          ylab = "Shannon index (ARGs)", xlab = "Shannon index (Species)")
dev.off()
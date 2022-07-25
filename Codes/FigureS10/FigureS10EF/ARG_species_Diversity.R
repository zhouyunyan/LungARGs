rm(list = ls())
setwd(".\\FigureS10\\FigureS10EF")

ARG <- read.delim("sample745_metadata_AROalpha.xls",header = T,check.names = F)
species <- read.delim("Bacteria_Species_Alpha_index_2.csv",header = T,check.names = F,sep=",")
names(species) <- c("Sample",paste("species",names(species)[-1],sep = "_"))

F7_ARG <- ARG[which(ARG$Population=="F7" &ARG$Site=="Lung"),c(1,6:10)]
names(F7_ARG) <- c("Sample",paste("ARG",names(F7_ARG)[-1],sep = "_"))
F7_species <- species[which(species$Sample %in% F7_ARG$Sample),]

data <- merge(F7_ARG,F7_species,by="Sample")

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
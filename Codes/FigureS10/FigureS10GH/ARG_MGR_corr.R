rm(list = ls())
setwd(".\\FigureS10GH")

MGR <- read.table("sample745_gene_richness.xls",header = T)
data <- read.delim("sample745_metadata_AROalpha.xls",header = T,check.names = F)

m <- merge(data,MGR,by="Sample")
m$MGR <- m$gene_richness/100000

##F7_Lung: 613 samples
F7_Lung <- m[which(m$Site=="Lung" & m$Population=="F7"),]
tiff(filename = "F7_Lung_ARG_Richness_MGR.tif",width = 1800,height = 1800,res=600,compression="lzw")
ggscatter(F7_Lung , x = "Number", y = "MGR", size = 1,
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "spearman",
          add.params = list(color = "blue", fill = "lightgray"), 
          xlab = "Richness (ARGs)", ylab = "MGR (¡Á 100,000)")
dev.off()

tiff(filename = "F7_Lung_ARG_Shannon_MGR.tif",width = 1800,height = 1800,res=600,compression="lzw")
ggscatter(F7_Lung , x = "Shannon", y = "MGR", 
          add = "reg.line", conf.int = TRUE, size = 1,
          cor.coef = TRUE, cor.method = "spearman",
          add.params = list(color = "blue", fill = "lightgray"), 
          xlab = "Shannon index (ARGs)", ylab = "MGR (¡Á 100,000)")
dev.off()

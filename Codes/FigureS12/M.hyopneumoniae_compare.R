rm(list = ls())
setwd(".\\FigureS12")

##1.metadata
library(readxl)
data <- read_xlsx("Lung_meta.xlsx",sheet = "metadata")
F7_Lung <- data[which(data$Site=="Lung" & data$Population=="F7"),]
F7_Lung$Grade <- factor(F7_Lung$Grade,levels = c("HL","SLL","MLL","SVLL"))

Bacteria <- read_xlsx("Tax_Species_split.xlsx",sheet = "Bacteria")
spe <- strsplit(Bacteria$Species,";")
spe_7 <- sapply(spe, "[",7)
Bacteria$Species <- spe_7

TopSpecies <- read.table("F7_Lung_TopMean_rela.xls",header = T,check.names = F)
Top_data <- Bacteria[which(Bacteria$Species %in% TopSpecies$Species),c("Species",F7_Lung$Sample)]
Top_data.t <- as.data.frame(t(Top_data[,-1])) 
names(Top_data.t) <- Top_data$Species

Top_data.t$Sample <- rownames(Top_data.t)
data.m <- merge(Top_data.t,F7_Lung[,c(1,5)],by="Sample")
data.m <- data.m[,c(1,26,2:25)]
data.m$Grade <- factor(data.m$Grade,levels = c("HL","SLL","MLL","SVLL"))

data.m.log <- log10(data.m[,3:26])
new.data <- as.data.frame(cbind(data.m[,1:2],data.m.log))

my_comparisons <- list(c("HL","SLL"),c("SLL","MLL"),c("MLL","SVLL"),
                       c("HL","MLL"),c("SLL","SVLL"),
                       c("HL","SVLL"))
label <- c("HL\n(n=51)","SLL\n(n=217)","MLL\n(n=218)","SVLL\n(n=127)")

tiff(filename = "M.hyopneumoniae.boxplot.tif",width = 2200,height = 2500,res=600,compression="lzw")
ggboxplot(new.data, x="Grade", y="Mycoplasma_hyopneumoniae",  color  = "Grade", add = "jitter",
          palette = "jco") + 
  border("grey")+
  labs(y="Relative abundance (log10)",x=NULL,title = "Mycoplasma hyopneumoniae")+
  scale_x_discrete("Grade",labels = label)+
  theme(axis.title.x = element_blank(),
        legend.title = element_blank(),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5,size = 13))+
  stat_compare_means(comparisons = my_comparisons,
                     label = "p.format",
                     tip.length = 0)
dev.off()

rm(list = ls())
setwd(".\\FigureS5")

library(readxl)
Bacteria <- read_xlsx("Tax_Species_split.xlsx",sheet = "Bacteria")
spe <- strsplit(Bacteria$Species,";")
spe_7 <- sapply(spe, "[",7)

library(vegan)
Freq <- specnumber(Bacteria[,-1])
mean_Abun <- rowMeans(Bacteria[,-1])

Freq_Abun <- as.data.frame(cbind(Bacteria$Species,spe_7,Freq,mean_Abun))
names(Freq_Abun)[1] <- "Species"
Freq_Abun[,3:4] <- lapply(Freq_Abun[,3:4],as.numeric)
Freq_Abun.order <- Freq_Abun[order(Freq_Abun$mean_Abun,decreasing = T),]
write.table(Freq_Abun.order,file = "Species_Freq_Abun.xls",row.names = F,sep = "\t")

#Top 20 Species
data <- Freq_Abun.order[1:20,]
data$mean_Abun <- as.numeric(format(data$mean_Abun,scipen=-5,digits = 2)) #format更改小数点显示位数
spe <- strsplit(data$Species,";")
phylum <- sapply(spe, "[",2)
data$phylum <- phylum
data$phylum <- factor(data$phylum,levels = c("Proteobacteria","Firmicutes","Tenericutes",
                                             "Bacteroidetes","Actinobacteria","Fusobacteria","Deinococcus-Thermus"))

library(ggpubr)
p <- ggdotchart(data, x = "spe_7", y = "mean_Abun",
           color = "phylum",                                # Color by groups
           #palette = c("#00AFBB", "#E7B800", "#FC4E07"), # Custom color palette
           palette = "npg",
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           #group = "phylum",                                # Order by groups
           dot.size = 6,                                 # Large dot size
           label = round(data$Freq),                        # Add Freq values as dot labels
           font.label = list(color = "white", size = 8,
                             vjust = 0.5),               # Adjust label parameters
           ggtheme = theme_pubr()                        # ggplot2 theme
) +
  ylim(0,0.011) +
  labs(x="Species",y="Relative Abundance",fill=NULL) +
  theme(legend.title = element_blank())

##set axis breaks
library(ggplot2)
library(ggbreak)
p2 <- p + scale_y_break(c(6e-04,0.010),scales="fixed")
pdf("species_abun_top20.pdf",width = 12,height = 6)
p2
dev.off()  

tiff(filename = "species_abun_top20.tif",width = 3600,height = 3000,res=600,compression="lzw")
p
dev.off()


##Percentage of Mycoplasma hyopneumoniae 
library(readxl)
Bacteria <- read_xlsx("Tax_Species_split.xlsx",sheet = "Bacteria")
rela <- sweep(Bacteria[,-1],2,colSums(Bacteria[,-1]),"/")
M.hyo <- t(rela[grep("Mycoplasma_hyopneumoniae",Bacteria$Species),])
mean(M.hyo)
# [1] 0.4627727


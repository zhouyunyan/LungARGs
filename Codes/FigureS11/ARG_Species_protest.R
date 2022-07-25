rm(list = ls())
setwd(".\\FigureS11")

data <- read.table(file = "F7_Lung_metadata_AROalpha.xls",header=T) 
ARO <-  read.delim(file = "sample745_ARO372_abundance.xls",header = T,row.names = 1,check.names = F)
ARO <- ARO[,names(ARO) %in% data$Sample]

library(readxl)
Bacteria <- read_xlsx("Tax_Species_split.xlsx",sheet = "Bacteria")
F7.lung.data <- Bacteria[,c("Species",names(ARO))]
spe <- strsplit(F7.lung.data$Species,";")
spe_7 <- sapply(spe, "[",7)
F7.lung.data$Species <- spe_7

F7.lung.data <- F7.lung.data[,-1]
rownames(F7.lung.data) <- spe_7

ARO.t <- t(ARO)
F7.lung.data.t <- t(F7.lung.data)

library(vegan)
A.distance <- vegdist(ARO.t,method = "bray") 
s.distance <- vegdist(F7.lung.data.t,method = "bray") 

A.distance.matrix <- as.matrix(A.distance)
A.distance.matrix.tr <- data.frame(row=rownames(A.distance.matrix)[row(A.distance.matrix)[upper.tri(A.distance.matrix)]],
                                   col=colnames(A.distance.matrix)[col(A.distance.matrix)[upper.tri(A.distance.matrix)]],
                                   distance=A.distance.matrix[upper.tri(A.distance.matrix)])
S.distance.matrix <- as.matrix(s.distance)
S.distance.matrix.tr <- data.frame(row=rownames(S.distance.matrix)[row(S.distance.matrix)[upper.tri(S.distance.matrix)]],
                                   col=colnames(S.distance.matrix)[col(S.distance.matrix)[upper.tri(S.distance.matrix)]],
                                   distance=S.distance.matrix[upper.tri(S.distance.matrix)])

##filter Mycoplasma_hyopneumoniae
col <- which(colnames(F7.lung.data.t)=="Mycoplasma_hyopneumoniae")
F7.lung.data.t.After <- F7.lung.data.t[,-col]
library(vegan)
s.distance.After <- vegdist(F7.lung.data.t.After,method = "bray") 

#NMDS
mds.A <- monoMDS(A.distance)
mds.s <- monoMDS(s.distance)
mds.s.After <- monoMDS(s.distance.After)

#protest analysis
set.seed(100)
pro.A.s <- procrustes(mds.A,mds.s)
protest(mds.A,mds.s)
# Call:
#   protest(X = mds.A, Y = mds.s) 
# 
# Procrustes Sum of Squares (m12 squared):        0.9765 
# Correlation in a symmetric Procrustes rotation: 0.1533 
# Significance:  0.001 
# 
# Permutation: free
# Number of permutations: 999

pro.A.s.After <- procrustes(mds.A,mds.s.After)
protest(mds.A,mds.s.After)
# Call:
#   protest(X = mds.A, Y = mds.s.After) 
# 
# Procrustes Sum of Squares (m12 squared):        0.6465 
# Correlation in a symmetric Procrustes rotation: 0.5945 
# Significance:  0.001 
# 
# Permutation: free
# Number of permutations: 999

##visualization
Y <- cbind(data.frame(pro.A.s$Yrot), data.frame(pro.A.s$X))
X <- data.frame(pro.A.s$rotation)
Y$ID <- rownames(Y)

library(ggplot2)
#Before filtration of M. hyopneumoniae
p.before <-ggplot(Y) +
  geom_segment(aes(x = X1, y = X2, xend = MDS1, yend = MDS2), 
               arrow = arrow(length = unit(0, 'cm')),
               color = "grey", size = 0.5) +
  geom_point(aes(MDS1, MDS2), fill = "#56B4E9", size = 1, shape = 21) +
  geom_point(aes(X1, X2), fill = "#B2182B", size = 1, shape = 21) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = 'transparent'),
        axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=12),
        axis.title.y=element_text(colour='black', size=12),
        axis.text=element_text(colour='black',size=10)) +
  labs(x = 'NMDS 1', y = 'NMDS 2', color = '') +
  #scale_color_manual(values = c('#56B4E9', '#B2182B'), limits = c('Species','ARGs')) +
  labs(title="Before filtration of M. hyopneumoniae") + 
  geom_vline(xintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_abline(intercept = 0, slope = X[1,2]/X[1,1], size = 0.3) +
  geom_abline(intercept = 0, slope = X[2,2]/X[2,1], size = 0.3) +
  annotate('text', label = 'r = 0.15, p = 0.001',
           x = -3.3, y = 2.3, size = 4,hjust = 0) +
  theme(plot.title = element_text(size=10,colour = "black",hjust = 0.5,face = "bold"))

addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 10, spaceLegend = 0.8) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(#legend.title = element_text(size = textSize,face = "bold",vjust = 0), 
      legend.text  = element_text(size = textSize),
      legend.key.size = unit(spaceLegend, "lines"))
}
tiff(filename = "ARG.species.protest.before.NMDS.tif",width = 2000,height = 2000,res=600,compression="lzw")
addSmallLegend(p.before)
dev.off()

pdf("ARG.species.protest.before.NMDS.pdf",width = 3.5,height = 3.5)
addSmallLegend(p.before)
dev.off()


#After filtration of M. hyopneumoniae
Y.after <- cbind(data.frame(pro.A.s.After$Yrot), data.frame(pro.A.s.After$X))
X.after <- data.frame(pro.A.s.After$rotation)
Y.after$ID <- rownames(Y.after)


p.after <-ggplot(Y.after) +
  geom_segment(aes(x = X1, y = X2, xend = MDS1, yend = MDS2), 
               arrow = arrow(length = unit(0, 'cm')),
               color = "grey", size = 0.5) +
  geom_point(aes(MDS1, MDS2), fill = "#56B4E9", size = 1, shape = 21) +
  geom_point(aes(X1, X2), fill = "#B2182B", size = 1, shape = 21) +
  theme(panel.grid = element_blank(), 
        panel.background = element_rect(color = 'black', fill = 'transparent'),
        legend.key = element_rect(fill = 'transparent'),
        axis.ticks.length = unit(0.4,"lines"), axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x=element_text(colour='black', size=12),
        axis.title.y=element_text(colour='black', size=12),
        axis.text=element_text(colour='black',size=10)) +
  labs(x = 'NMDS 1', y = 'NMDS 2', color = '') +
  #scale_color_manual(values = c('#56B4E9', '#B2182B'), limits = c('Species','ARGs')) +
  labs(title="After filtration of M. hyopneumoniae") + 
  geom_vline(xintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_hline(yintercept = 0, color = 'gray', linetype = 2, size = 0.3) +
  geom_abline(intercept = 0, slope = X[1,2]/X[1,1], size = 0.3) +
  geom_abline(intercept = 0, slope = X[2,2]/X[2,1], size = 0.3) +
  annotate('text', label = 'r = 0.57, p = 0.001',
           x = -3.3, y = 2.3, size = 4,hjust = 0) +
  theme(plot.title = element_text(size=10,colour = "black",hjust = 0.5,face = "bold"))

addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 10, spaceLegend = 0.8) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(#legend.title = element_text(size = textSize,face = "bold",vjust = 0), 
      legend.text  = element_text(size = textSize),
      legend.key.size = unit(spaceLegend, "lines"))
}
tiff(filename = "ARG.species.protest.after.NMDS.tif",width = 2000,height = 2000,res=600,compression="lzw")
addSmallLegend(p.after)
dev.off()

pdf("ARG.species.protest.after.NMDS.pdf",width = 3.5,height = 3.5)
addSmallLegend(p.after)
dev.off()
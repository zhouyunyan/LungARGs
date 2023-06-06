rm(list = ls())

library(readxl)
##Tn916
data1 <- read_xlsx("for_plot.xlsx",sheet = "tet_Tn916")
data1$Name <- factor(data1$Name,levels = unique(data1$Name)[order(unique(data1$Name),decreasing = T)])

library(ggplot2)
library(gggenes)
p <- ggplot(data1, aes(xmin = Start, xmax = End, 
                  y = Name, fill = Type,
                  label= Gene)) +
  geom_gene_arrow() +
  facet_wrap(~ Name, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()+
  geom_gene_label(align = "centre")


tiff(filename = "tet_Tn916.tif",width = 5800,height = 2800,res=600,compression="lzw")
p
dev.off()

pdf(file = "tet_Tn916.pdf",width = 10,height =7)
p
dev.off()

##tnpA
data2 <- read_xlsx("for_plot.xlsx",sheet = "tnpA")
data2$Name <- factor(data2$Name,levels = unique(data2$Name)[order(unique(data2$Name),decreasing = T)])

library(ggplot2)
library(gggenes)
p <- ggplot(data2, aes(xmin = Start, xmax = End, 
                       y = Name, fill = Type,
                       label= Gene)) +
  geom_gene_arrow() +
  facet_wrap(~ Name, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()+
  geom_gene_label(align = "centre")


tiff(filename = "tnpA.tif",width = 5800,height = 2800,res=600,compression="lzw")
p
dev.off()

pdf(file = "tnpA.pdf",width = 10,height =7)
p
dev.off()


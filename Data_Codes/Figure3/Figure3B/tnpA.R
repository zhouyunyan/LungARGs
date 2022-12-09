rm(list = ls())

library(readxl)
data1 <- read_xlsx("tnpA.xlsx",sheet = "tnpA")
data1$ContigID <- factor(data1$ContigID,levels = rev(unique(data1$ContigID)))
data1$Data <- factor(data1$Data,levels = unique(data1$Data))

library(ggplot2)
library(gggenes)
p <- ggplot(data1, aes(xmin = Start, xmax = End, 
                       y = ContigID, fill = Type,
                       label= Gene)) +
  geom_gene_arrow() +
  facet_wrap(~ Data, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()+
  geom_gene_label(align = "centre")

tiff(filename = "tnpA.tif",width = 4600,height = 3200,res=600,compression="lzw")
p
dev.off()

pdf(file = "tnpA.pdf",width = 8,height =6)
p
dev.off()
rm(list = ls())

library(readxl)
data1 <- read_xlsx("tetL_repUS12.xlsx",sheet = "tetL_rep")
data1$ContigID <- factor(data1$ContigID,levels = rev(unique(data1$ContigID)))

library(ggplot2)
library(gggenes)
p <- ggplot(data1, aes(xmin = Start, xmax = End, 
                  y = ContigID, fill = Type,
                  label= Gene)) +
  geom_gene_arrow() +
  #facet_wrap(~ Data, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()+
  geom_gene_label(align = "centre")

tiff(filename = "tetL_repUS12.tif",width = 4600,height = 500,res=600,compression="lzw")
p
dev.off()

pdf(file = "tetL_repUS12.pdf",width = 8,height =1)
p
dev.off()
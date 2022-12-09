rm(list = ls())

library(readxl)
data1 <- read_xlsx("MAG_E.coli.xlsx",sheet = "MAG_E.coli")
data1$strand <- rep("NA",nrow(data1))

data1$strand[which(data1$Start<data1$End)] <- "forward"
data1$strand[which(data1$Start>data1$End)] <- "reverse"


library(ggplot2)
library(gggenes)

tiff(filename = "E.coli_gene_structure_2.tif",width = 4500,height = 2000,res=600,compression="lzw")
ggplot(data1, aes(xmin = Start, xmax = End, 
                  y = ContigID, fill = Type,
                  label= Gene)) +
  geom_gene_arrow() +
  facet_wrap(~ ContigID, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  theme_genes()+
  geom_gene_label(align = "centre")
dev.off()

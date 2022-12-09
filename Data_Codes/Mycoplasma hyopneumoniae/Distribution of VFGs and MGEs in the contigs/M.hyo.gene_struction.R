rm(list = ls())

library(readxl)
data1 <- read_xlsx("M.hyo_MGE_result.xlsx",sheet = "gene_stru")
data1$strand <- rep("NA",nrow(data1))

data1$strand[which(data1$Start<data1$End)] <- "forward"
data1$strand[which(data1$Start>data1$End)] <- "reverse"

data1$Type <- factor(data1$Type, levels = c("MGE","VF","prophage") )
library(ggplot2)
library(gggenes)

tiff(filename = "M.hyo_gene_structure.tif",width = 4500,height = 2000,res=600,compression="lzw")
ggplot(data1, aes(xmin = Start, xmax = End, 
                  y = ContigID, fill = Type,
                  label= Gene)) +
  geom_gene_arrow() +
  facet_wrap(~ ContigID, scales = "free", ncol = 1) +
  scale_fill_brewer(palette = "Set3") +
  
  theme_genes()+
  geom_gene_label(align = "centre")
dev.off()
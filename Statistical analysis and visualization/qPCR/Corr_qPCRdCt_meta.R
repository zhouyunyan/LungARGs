rm(list = ls())

data.m <- read.delim("qPCR_meta_ARG_MGE_MAG.xls",header=T,row.names=1,check.names=F,sep = "\t")

library(ggpubr)
data.m$MAG_26.1 <- data.m$MAG_26/100
p <- ggscatter(data.m, y = "MAG_26.1", x = "dCtMAG26", 
               add = "reg.line", conf.int = TRUE, size = 1,
               add.params = list(color = "blue", fill = "lightgray"),
               cor.coef = TRUE, cor.method = "spearman",
               cor.coeff.args = list(method = "spearman"),
               ylab = "MAG_26", xlab = "dCt MAG_26")

tiff(filename = "qPCR_Fig\\dCt. MAG_26_MAG_26.1.tif",width = 1800,height = 1500,res=600,compression="lzw")
p
dev.off()

pdf(file = "qPCR_Fig\\dCt. MAG_26_ MAG_26.1.pdf",width = 4,height =3)
p
dev.off()


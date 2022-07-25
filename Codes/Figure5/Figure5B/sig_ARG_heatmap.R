rm(list = ls())
setwd(".\\Figure5\\Figure5B")

data <- read.delim("compare.sig.ARG_Abun.xls",header = T,row.names = 1,check.names = F,sep = "\t")
metadata <- read.delim("compare.sig.ARG_info.xls",header = T,row.names = 1,check.names = F,sep = "\t")
library(plyr)
group_abun <- ddply(data,"Grade",numcolwise(mean))
dt <- as.data.frame(t(group_abun))
names(dt) <- c("HL","MLL","SLL","SVLL")
dt <- dt[-1,c(1,3,2,4)]
dt[,1:4] <- lapply(dt[,1:4], as.numeric)
rownames(dt)[c(18,33,47)] <- c("C.coli_CC","Hinf_PBP3_BLA","catB7")

library(pheatmap)
library(gplots)
library(ggplot2)
anno <- as.data.frame(metadata[,c(3,4)])
rownames(anno)[c(18,33,47)] <- c("C.coli_CC","Hinf_PBP3_BLA","catB7")
names(anno) <- c("Drug","Mechanism")
write.table(dt,file = "sig_ARG_group_Abun.xls",sep = "\t")


tiff(filename = "sig.ARG.pheatmap.tif",width = 4500,height = 5500,res=600,compression="lzw")
pheatmap(dt,scale = "row",cluster_rows =T,cluster_cols =F,
         color = bluered(100),fontsize=10,
         annotation_row  = anno,
         annotation_legend=T)
dev.off()

#Don't show legend
tiff(filename = "sig.ARG.pheatmap1.tif",width = 3000,height = 5500,res=600,compression="lzw")
pheatmap(dt,scale = "row",cluster_rows =T,cluster_cols =F,
         color = bluered(100),fontsize=10,
         annotation_row  = anno,
         annotation_legend=F)
dev.off()

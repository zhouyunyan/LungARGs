rm(list = ls())
setwd(".\\Figure6\\Figure6B")

##pheatmap
result.m <- read.table("M.hypo_7_MGE_5.xls",header = T,sep = "\t",row.names = 1,check.names = F)
result.count.m <- read.table("M.hypo_7_MGE_5_countNum.xls",header = T,sep = "\t",row.names = 1,check.names = F)

dt <- result.m[,2:8]
dt[,1:7] <- lapply(dt[,1:7],as.numeric)
anno <- as.data.frame(result.m[,1])
names(anno) <- "MGE Types"
rownames(anno) <- rownames(dt)

#add VF number
dt.num <- as.matrix(result.count.m[,-1]) 

library(pheatmap)
library(gplots)
library(ggplot2)
tiff(filename = "M.hyo_7_MGE_5.pheatmap.tif",width = 3500,height = 2000,res=600,compression="lzw")
pheatmap(dt,cluster_rows =T,cluster_cols =F,
         color = bluered(100),fontsize=12,
         legend = F,
         display_numbers = matrix(ifelse((dt.num > 1 & !is.na(dt.num)),dt.num,""),nrow(dt.num)),
         annotation_row  = anno,
         annotation_legend=T)
dev.off()
rm(list = ls())

ARG <- read.delim("MAG_ARG_merge.xls",header = T,check.names = F,sep = "\t")
ARG_sub <- ARG[order(ARG$Best_Hit_ARO),1:2]
names(ARG_sub)[2] <- "ARG_name"

ARG_sub_filter <- unique(ARG_sub)

#Count the number of host bacteria of each ARG
ARG_Freq <- as.data.frame(table(ARG_sub_filter$ARG_name))
ARG_Freq <- ARG_Freq[order(ARG_Freq$Freq,decreasing = T),]
write.table(ARG_Freq,file = "ARG_host_num.xls",sep = "\t",row.names = F)

Top_ARG <- as.character(ARG_Freq$Var1[which(ARG_Freq$Freq>=5)])
Top <- ARG_sub_filter[which(ARG_sub_filter$ARG_name %in% Top_ARG),]
MAG <- unique(Top$MAG_ID)
result <- NULL
YN <- rep("NA",length(MAG))
for (i in 1:length(Top_ARG)) {
  name <- Top$MAG_ID[which(Top$ARG_name==Top_ARG[i])]
  YN[which(MAG %in% name)] <- 1
  YN[which(!(MAG %in% name))] <- 0
  result <- cbind(result,YN)
}
result <- as.data.frame(result)
colnames(result)<- Top_ARG
colnames(result)[5:6]<- c("E.cloi_EF-Tu","Hinf_PBP3_BLA")
result[,1:9] <- lapply(result[,1:9], as.numeric)
rownames(result) <- MAG
write.table(result,file = "topARG_9_MAG_77_matrix.xls",sep = "\t",row.names = T)

#taxonomy annotation
library(readxl)
taxa <- read_xlsx("MAG_ARG.xlsx",sheet="MAG_115")
anno <- taxa[match(MAG,taxa$MAG_ID),]
label <- rep("NA",nrow(anno))
label[which(!is.na(anno$Species))] <- anno$Species[which(!is.na(anno$Species))]
label[which(is.na(anno$Species) & !(is.na(anno$Genus)))] <- anno$Genus[which(is.na(anno$Species) & !(is.na(anno$Genus)))]
label[which(is.na(anno$Genus) & !(is.na(anno$Family)))] <- anno$Family[which(is.na(anno$Genus) & !(is.na(anno$Family)))]
label <- as.data.frame(label)
rownames(label) <- anno$MAG_ID
write.table(label,file = "MAG_77_taxa.xls",sep = "\t",row.names = T)

#new MAG ID
library(readxl)
info <- read_xlsx("MAG_ARG.xlsx",sheet="MAG_397_info")
label$newID <- info$MAG_rename[match(rownames(label),info$genome2)]
label$new_name <- paste(label$newID,label$label,sep = "-")

library(pheatmap)
library(gplots)
library(ggplot2)
rownames(result) <- label$new_name[match(rownames(result),rownames(label))]
tiff(filename = "TopARG_9_MAG_77.pheatmap1.tif",width = 4500,height = 6000,res=600,compression="lzw")
pheatmap(result,scale = "none",cluster_rows =T,cluster_cols =F,
         color = bluered(100),fontsize=10, angle_col=45,
         #annotation_row  = anno,
         annotation_legend=F)
dev.off()

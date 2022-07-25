rm(list = ls())
setwd(".\\Figure4\\Figure4C")

metadata <- read.table("Lung2Trachea.xls",header = T)

ARG <- read.delim("sample745_ARO372_abundance.xls",header=T,row.names=1,check.names=F,sep = "\t")
site_ARG <- ARG[,which(names(ARG) %in% metadata$Sample)]

Lung_ARG <- site_ARG[,metadata$Sample[which(metadata$Site=="Lung")]]
Trachea_ARG <- site_ARG[,metadata$Sample[which(metadata$Site=="Trachea")]]

library(vegan)
#Prevalence
Lung_Freq <-specnumber(Lung_ARG)
Trachea_Freq <-specnumber(Trachea_ARG)

df_Freq <- data.frame(sample=rownames(site_ARG),Lung_Freq=Lung_Freq,Trachea_Freq=Trachea_Freq)
df_Freq$sum <- rowSums(df_Freq[,-1])
df_Freq <- df_Freq[order(df_Freq$sum,decreasing = T),]
write.table(df_Freq,file = "ARG_Site_Freq.xls",row.names = F,quote = F,sep = "\t")

Site <- as.data.frame(t(site_ARG))
Site$Sample <-  rownames(Site)
Site.data <- merge(metadata[,c(1,2)],Site,by="Sample")
Site.melt <- melt(Site.data,id.vars = c("Sample","Site"))
Site.compare <- compare_means(value ~ Site, data=Site.melt, 
                         group.by ="variable", p.adjust.method = "fdr")
compare.sig <- Site.compare[which(Site.compare$p.adj<0.05),]
write.table(compare.sig,file = "Site.compare.sig.ARG.xls",sep = "\t",quote = F,row.names = F)

#The abundance of ARGs that significantly different between two sites
sig <- as.character(unique(compare.sig$variable)) 
sig_data <- Site.data[,c(1,2,which(names(Site.data) %in% sig))]
write.table(sig_data,file = "Site.compare.sig.ARG_Abun.xls",sep = "\t",quote = F,row.names = F)

##boxplot
data <- read.delim("Site.compare.sig.ARG_Abun.xls",header = T,row.names = 1,check.names = F,sep = "\t")
colnames(data)[grep("folC",colnames(data))] <- "folC"

library(reshape2)
dt.m <- melt(data,id.vars = "Site")
dt.m$value.log <- log10(dt.m$value) 
library(ggpubr)
library(ggplot2)
p1 <- ggboxplot(dt.m, x="variable", y="value.log", fill = "Site",
                order = sort(unique(dt.m$variable),decreasing = T)) + 
  border("grey")+
  labs(x=NULL,y="Relative abundance (log10)",colour ="Site")+
  coord_flip()  


tiff(filename = "Site.sig.ARG.boxplot.tif",width = 2200,height = 4000,res=600,compression="lzw")
p1
dev.off()

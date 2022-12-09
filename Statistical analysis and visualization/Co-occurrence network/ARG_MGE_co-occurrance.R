rm(list = ls())

ARG <- read.delim("sample745_ARO372_abundance.xls",header=T,row.names=1,check.names=F,sep = "\t")
MGE <- read.delim("MGE_Type_83_abun.xls",header=T,row.names=1,check.names=F,sep = "\t")
MGE.dt <- MGE[,-1]

ARG.t <- as.data.frame(t(ARG))
MGE.t <- as.data.frame(t(MGE.dt))

library(psych)
corr <- corr.test(ARG.t, MGE.t,use = "pairwise",method="spearman",adjust="fdr")
r <- corr$r
p <- corr$p.adj


RESULT <- c("ARG", "MGE", "r", "p")
for (i in 1:ncol(p)){
  for(j in 1:nrow(p)){
    if(p[j,i]< 0.05){
      P_Result <- p[j,i]
      Cor_Result <- r[j,i]
      tmp <- cbind(rownames(p)[j], colnames(p)[i], Cor_Result, P_Result)
      RESULT <- rbind(RESULT, tmp)
    }
  }
}
colnames(RESULT) <- RESULT[1,]
RESULT <- as.data.frame(RESULT[-1, ])
RESULT[,3:4] <- lapply(RESULT[,3:4],as.numeric)
final <- RESULT[which(abs(RESULT$r)>=0.5),]
write.csv(final, "ARG_MGE_COR0.5_p0.05.csv", row.names = F, quote = F)

##For network
uniqe.ARG <- unique(final$ARG)
uniqe.MGE <- unique(final$MGE)

#The types of ARGs and MGEs
MGE_Type <- cbind(subtype=rownames(MGE),type=MGE$MGE_Class)
ARG_Type.data <- read.delim("ARO_Freq_Abun.xls",header=T,check.names=F,sep = "\t")
ARG_Type <-  ARG_Type.data[,c(1,4)]
names(ARG_Type) <- c("subtype","type")
Type.data <- as.data.frame(rbind(ARG_Type,MGE_Type)) 

#The abundances of ARGs and MGEs
ARG.Abun <- ARG_Type.data[,c(1,8)]
names(ARG.Abun) <- c("Name","MeanAbun")
MGE.Abun <- data.frame(Name=rownames(MGE.dt),MeanAbun=rowMeans(MGE.dt))
Abun.data <- as.data.frame(rbind(ARG.Abun,MGE.Abun)) 

Node <- c(uniqe.ARG,uniqe.MGE)
Type <- Type.data$type[match(Node,Type.data$subtype)]
Abun <- Abun.data$MeanAbun[match(Node,Abun.data$Name)]
data.type <- rep(c("ARGs","MGEs"),c(length(uniqe.ARG),length(uniqe.MGE)))

data <- data.frame(Node,Type,Abun,data.type,stringsAsFactors = F)
write.csv(data,file = "ARG_MGE_info_network.csv", row.names = F, quote = F)


rm(list = ls())
setwd(".\\FigureS6")

library(readxl)
data <- read_xlsx("focus_ARGs.xlsx",sheet = "focus_ARGs") 
num_ORF <- table(data$ARO_Name)
# adeF  APH(6)-Id       floR    tet(39)     tet(D)     tet(L) tet(W/N/W)  tetQ
# 57         12         14          7         13         16         43    35

data_contig <- unique(data[,-1])
num_contig <- table(data_contig$ARO_Name)
# adeF  APH(6)-Id       floR    tet(39)     tet(D)     tet(L) tet(W/N/W) tetQ
# 57         12         14          7         13         16         43    34


Freq <- aggregate(data_contig$ContigID,by=list(data_contig$contig_kraken2,data_contig$ARO_Name),length)
names(Freq) <- c("Taxa","ARO_Name","Freq")

Tax <- strsplit(Freq$Taxa,"|",fixed = T)
result <- NULL
for (i in 1:7) {
  level <- sapply(Tax, "[",i)
  result <- cbind(result,level)
}
result <- data.frame(Freq$ARO_Name, result,Freq$Freq)
names(result) <- c("ARG","Kingdom","Phylum","Class","Order","Family","Genus","Species","Freq")

##folR
result <- result[which(result$ARG=="floR"),]

#rename NA/unknown,such as p_unknown
result$Phylum[which(is.na(result$Phylum) | result$Phylum=="unknown")] <- "p_unknown"
result$Class[which(is.na(result$Class) | result$Class=="unknown")] <- "c_unknown"
result$Order[which(is.na(result$Order) | result$Order=="unknown")] <- "o_unknown"
result$Family[which(is.na(result$Family) | result$Family=="unknown")] <- "f_unknown"
result$Genus[which(is.na(result$Genus) | result$Genus=="unknown")] <- "g_unknown"
result$Species[which(is.na(result$Species) | result$Species=="unknown")] <- "s_unknown"

#construct link-list for Sankey plot
ARG_phylum <- aggregate(result$Freq, by = list(result$ARG,result$Phylum), FUN = sum)
names(ARG_phylum) <- c('source', 'target', 'Freq')
phylum_class <- aggregate(result$Freq, by = list(result$Phylum,result$Class), FUN = sum)
names(phylum_class) <- c('source', 'target', 'Freq')
class_order <- aggregate(result$Freq, by = list(result$Class,result$Order), FUN = sum)
names(class_order) <- c('source', 'target', 'Freq')
order_family <- aggregate(result$Freq, by = list(result$Order,result$Family), FUN = sum)
names(order_family) <- c('source', 'target', 'Freq')
family_genus <- aggregate(result$Freq, by = list(result$Family,result$Genus), FUN = sum)
names(family_genus) <- c('source', 'target', 'Freq')
genus_species <- aggregate(result$Freq, by = list(result$Genus,result$Species), FUN = sum)
names(genus_species) <- c('source', 'target', 'Freq')

link_list <- rbind(ARG_phylum,phylum_class,class_order,order_family,family_genus,genus_species)
unknown_row <- unique(c(grep("unknown",link_list$source,fixed = F),
                        grep("unknown",link_list$target,fixed = F)))
link_list <- link_list[-unknown_row,]
#construct node list and assign id for each taxon
node_list <- reshape2::melt(result[,-2], id = 'Freq')
node_list <- node_list[!duplicated(node_list$value), ]
node_list <- node_list[-grep("unknown",node_list$value,fixed = F),]

link_list$IDsource <- match(link_list$source, node_list$value) - 1 
link_list$IDtarget <- match(link_list$target, node_list$value) - 1

#visualization
library(networkD3)
p <- sankeyNetwork(Links = link_list, Nodes = node_list,
                   Source = 'IDsource', Target = 'IDtarget', Value = 'Freq', 
                   NodeID = 'value', NodeGroup = 'variable', 
                   fontSize = 20,
                   sinksRight = FALSE,width = 1700,height = 300)

p
saveNetwork(p,"floR_ARG_Taxa_Sankey.html")
library(webshot)
webshot("floR_ARG_Taxa_Sankey.html","floR_ARG_Taxa_Sankey.pdf",vwidth = 600,vheight=300)

rm(list = ls())

taxa <- read.delim("Kraken2_level.data.xls",header = T,check.names = F,sep = "\t")
data <- taxa[,-1]

#split taxa
Species <- data[which(!is.na(data$Species)),]
Genus <- data[which((!is.na(data$Genus)) & is.na(data$Species)),]
Family <- data[which((!is.na(data$Family)) & is.na(data$Genus)),]
Order <- data[which((!is.na(data$Order)) & is.na(data$Family)),]
Class <- data[which((!is.na(data$Class)) & is.na(data$Order)),]
Phylum <- data[which((!is.na(data$Phylum)) & is.na(data$Class)),]
Kingdom <- data[which((!is.na(data$Domain)) & is.na(data$Phylum)),]

#construct link-list for Sankey plot
genus_species <- Species[,c('Genus','Species','Freq')]
names(genus_species) <- c('source', 'target', 'Freq')
family_genus <- Genus[,c('Family','Genus','Freq')]
names(family_genus) <- c('source', 'target', 'Freq')
order_family <- Family[,c('Order','Family','Freq')]
names(order_family) <- c('source', 'target', 'Freq')
class_order <- Order[,c('Class','Order','Freq')]
names(class_order) <- c('source', 'target', 'Freq')
phylum_class <- Class[,c('Phylum','Class','Freq')]
names(phylum_class) <- c('source', 'target', 'Freq')
kingdom_phylum <- Phylum[,c('Domain','Phylum','Freq')]
names(kingdom_phylum) <- c('source', 'target', 'Freq')


species_save <- genus_species[order(genus_species$Freq,decreasing = T),]
write.table(species_save,file = "Kraken2_species_num.xls",row.names = F,sep = "\t",quote = F)

link_list <- rbind(kingdom_phylum,phylum_class, class_order, order_family, family_genus,genus_species)
write.table(link_list,file = "Kraken2_link_list_sankey.xls",row.names = F,sep = "\t",quote = F)

link_list <- link_list[which(link_list$Freq >5),]

#construct node list and assign id for each taxon
node_list <- reshape2::melt(data, id = 'Freq')
node_list <- node_list[!duplicated(node_list$value), ]
node_list <- node_list[-which(is.na(node_list$value)),]
node_list <- node_list[which(node_list$value %in% unique(c(link_list$source,link_list$target))),]

link_list$IDsource <- match(link_list$source, node_list$value) - 1 
link_list$IDtarget <- match(link_list$target, node_list$value) - 1

#networkD3 
library(networkD3)
p <- sankeyNetwork(Links = link_list, Nodes = node_list,
                   Source = 'IDsource', Target = 'IDtarget', Value = 'Freq', 
                   NodeID = 'value', NodeGroup = 'variable', 
                   fontSize = 20, sinksRight = FALSE,width = 1600,height = 1000)

p

#save to .html
saveNetwork(p,"Kraken2_contig_Sankey.html")

library(webshot)
webshot("Kraken2_contig_Sankey.html","Kraken2_contig_Sankey.pdf",vwidth = 1000,vheight=1000)


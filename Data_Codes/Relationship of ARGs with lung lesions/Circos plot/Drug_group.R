rm(list = ls())

Abun_data <- read.delim("Drug_group.xls",header = T,row.names = 1,check.names = F)
names(Abun_data) <- c("HL","SLL","MLL","SVLL")
order <- rownames(Abun_data)[order(rowMeans(Abun_data),decreasing = T)]
Top <- Abun_data[match(order[1:7],rownames(Abun_data)),]
Top <- as.matrix(Top) 
  
library(statnet)
library(circlize)
Top <- Top/0.00001 #Adjust the coordinate scale

library("ggsci")
mypal =pal_jco("default", alpha =1)(4)
mypal_drug <- pal_npg("nrc", alpha =1)(nrow(Top)+1)

grid.col = NULL
grid.col[colnames(Top)] = mypal
grid.col[rownames(Top)] = mypal_drug[-1]

tiff(filename = "F7_Lung_circos.tif",width = 2800,height = 2900,res=600,compression="lzw")
chordDiagram(Top,grid.col = grid.col,
             directional = F,diffHeight = 0.3,
             transparency = 0.4, annotationTrack =c("grid", "axis"),
             preAllocateTracks = 2)

circos.track(track.index = 1, panel.fun = function(x, y) {
  circos.text(CELL_META$xcenter, CELL_META$ylim[1], CELL_META$sector.index,
              facing = "clockwise", cex = 0.8, niceFacing = TRUE, adj = c(0.3, 0))}, 
  bg.border = NA) # here set bg.border to NA is important

circos.clear()
dev.off()

rm(list = ls())

data <- read.delim(file = "ARO_Freq_Abun.xls",header = T,sep = "\t",check.names = F)

data$Abun.log<- log10(data$MeanAbun)

mean_abun <- mean(data$MeanAbun)
data$group <- rep(NA,nrow(data))

data$group[which(data$Freq >=559 & data$MeanAbun >= mean_abun)] <- "Freq ¡İ 75%, ¡İ Average abundance"
data$group[which(data$Freq >=75 & data$Freq < 559 & data$MeanAbun >= mean_abun)] <- "10% ¡Ü Freq < 75%, ¡İ Average abundance"
data$group[which(data$Freq >=75 & data$Freq < 559 & data$MeanAbun < mean_abun)] <- "10% ¡Ü Freq < 75%, < Average abundance"
data$group[which(data$Freq < 75 & data$MeanAbun < mean_abun)] <- "Freq < 10%, < Average abundance"

data$group <- factor(data$group,levels = c("Freq ¡İ 75%, ¡İ Average abundance","10% ¡Ü Freq < 75%, ¡İ Average abundance",
"10% ¡Ü Freq < 75%, < Average abundance","Freq < 10%, < Average abundance"))

library(ggplot2)
library(ggrepel) 

data$label <- data$ARO_Name 
data$label[which(!(data$group == "Freq ¡İ 75%, ¡İ Average abundance" | data$ARO_Name %in% c("tet(39)","tet(L)")))] <- ""
p <- ggplot(data,aes(x=Freq,y=Abun.log,color=group))+ 
  geom_point(alpha=0.5,size=1)+
  theme_bw()+
  theme(panel.grid.major = element_line(colour = NA),
        axis.title= element_text(size = 12,color="black"),
        axis.text = element_text(size = 10,color="black"),
        legend.title=element_blank(),
        legend.position = c(0.60,0.16))+
  #legend.box.background = element_rect(color="grey", size=0.5))
  labs(x="Number of samples",y="ARG abundance (log10)")+
  geom_text_repel(aes(label=label),size = 1.6,segment.size=0.1,
                  segment.colour = "grey",min.segment.length = 0,
                  max.overlaps = getOption("ggrepel.max.overlaps", default = 12))

##legend
addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 8, spaceLegend = 0.7) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize))) +
    theme(legend.title = element_blank(), 
          legend.text  = element_text(size = textSize),
          legend.key.size = unit(spaceLegend, "lines"))
}

tiff(filename = "ARO.Abun.sampleNum.tif",width = 2200,height = 2000,res=600,compression="lzw")
addSmallLegend(p)
dev.off()
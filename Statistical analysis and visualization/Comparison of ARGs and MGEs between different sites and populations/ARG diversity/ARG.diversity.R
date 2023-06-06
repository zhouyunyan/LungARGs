rm(list = ls())

path = ".\\ARG diversity"
input1 = "sample745_metadata_AROalpha.xls"
input2 = "sample745_ARO372_abundance.xls"
input3 = "Lung2Trachea.xls"

setwd(path)
data1 <- read.table(file = input1,header=T)


p.diversity <- function(data, x, y, title.x, title.y, label, my_comparisons) {

  library(ggpubr)
  ggboxplot(data, x=x, y=y,  fill  = x, add = "jitter",
                       palette = "png") + 
    guides(fill="none")+
    labs(x=title.x,y=title.y)+
    border("grey")+
    scale_x_discrete(x,labels = label)+
    theme(axis.title.x = element_blank())+
    stat_compare_means(comparisons = my_comparisons,
                       label = "p.format",
                       tip.length = 0)
}

group <- c("Population","Site")
value <- c("Number", "Shannon", "ARO_Abun")
title.x <- NULL
title.y <- c("Richness (ARGs)","Shannon index (ARGs)","Abundance (ARGs)")

##different population
var <- c("F7","Shanxia","Changzhou","Zangzhu","Yezhu")
sub_data <- data1[which(data1$Site=="Lung"),]
sub_data[,group[1]] <- factor(sub_data[,group[1]],levels = var)
my_comparisons <- list(c("Yezhu","Zangzhu"),c("Yezhu","Changzhou"),c("Yezhu","Shanxia"),c("Yezhu","F7"),
                       c("Zangzhu","Changzhou"),c("Zangzhu","Shanxia"),c("Zangzhu","F7"))
label <- c("F7\n(n=613)","BL\n(n=28)","Erhualian\n(n=9)","Tibetan\n(n=11)","Wild boars\n(n=9)")
label2 <- c("F7 (n=613)","BL (n=28)","Erhualian (n=9)","Tibetan (n=11)","Wild boars (n=9)")

Number <- p.diversity(sub_data, group[1], value[1], title.x, title.y[1], label, my_comparisons)
Shannon <- p.diversity(sub_data, group[1], value[2], title.x, title.y[2], label, my_comparisons)
Abun <- p.diversity(sub_data, group[1], value[3], title.x, title.y[3], label, my_comparisons)

#different site
data.tmp <- read.table(file = input3,header=T)
var <- c("Lung","Trachea")
sub_data <- data1[which(data1$Sample%in% data.tmp$Sample),]
sub_data[,group[2]] <- factor(sub_data[,group[2]],levels = var)
my_comparisons <- list(c("Lung","Trachea"))
label <- c("Lung","Trachea")

Number <- p.diversity(sub_data, group[2], value[1], title.x, title.y[1], label, my_comparisons)
Shannon <- p.diversity(sub_data, group[2], value[2], title.x, title.y[2], label, my_comparisons)
Abun <- p.diversity(sub_data, group[2], value[3], title.x, title.y[3], label, my_comparisons)

p.Alpha <- list(Number, Shannon, Abun)

##plot
p.save <- function(group,width,height){
for (i in 1:length(value)) {
  filename <- paste(paste(group,"ARG",sep = "_"),value[i],sep = ".")
  #tif
  ggsave(plot = p.Alpha[[i]], filename = paste(filename,"tiff",sep = "."),width = width, height = height)
  #pdf
  ggsave(plot = p.Alpha[[i]], filename = paste(filename,"pdf",sep = "."),width = width, height = height)
}
}

p.save(group[1], 4.5, 4) #Population
p.save(group[2], 2.5, 4) #Site

##Beta Diversity
data2 <- read.delim(input2,header = T,row.names = 1,check.names = F)
data <- as.data.frame(t(data2[,sub_data$Sample]))
#population
data[,group[1]] <- sub_data[,group[1]][match(rownames(data),sub_data$Sample)]
var <- c("F7","Shanxia","Changzhou","Zangzhu","Yezhu")
data.split <- split(data,data[,group[1]])

#Site
data[,group[2]] <- sub_data[,group[2]][match(rownames(data),sub_data$Sample)]
var <- c("Lung","Trachea")
data.split <- split(data,data[,group[2]])

library(vegan)
dt.distance <- NULL
Group <- NULL
for (i in 1:length(data.split)) {
  #dt <- data[which(data[,group[1]] == var[i]),-ncol(data)]
  dt <- data.split[[i]][,-ncol(data)]
  distance <- vegdist(dt,method="bray")
  length <- length(distance)
  tmp <- rep(var[i],length)
  dt.distance <- c(dt.distance,distance)
  Group <- c(Group,tmp)
}
distance.data <- data.frame(Group,dt.distance)

p.beta <- function(data, x, y, title.x, title.y, label, my_comparisons){
  ggviolin(data, x=x, y=y,  fill  = x, add = "boxplot",
         palette = "png") + 
  guides(fill="none")+
  labs(y=title.y, x=title.x)+
  border("grey")+
  scale_x_discrete(x,labels = label)+
  theme(axis.title.x = element_blank(),
        legend.position = "none")+
  stat_compare_means(comparisons = my_comparisons,
                     label = "p.format",
                     tip.length = 0)
}

Beta <- p.beta(distance.data,"Group","dt.distance",NULL,"Beta Diversity (ARGs)",label, my_comparisons)

p.save <- function(group,width,height){
  filename <- paste(paste(group,"ARG",sep = "_"),"Beta",sep = ".")
  #tif
  ggsave(plot = Beta, filename = paste(filename,"tiff",sep = "."),width = width,height = height)
  #pdf
  ggsave(plot = Beta, filename = paste(filename,"pdf",sep = "."),width = width, height = height)
}

p.save(group[1], 4.5, 4) #Population
p.save(group[2], 2.5, 4) #Site

##PCoA
data2 <- read.delim(input2,header = T,row.names = 1,check.names = F)
data3 <- as.data.frame(t(data2[,sub_data$Sample]))
#ANOSIM analysis
#Population
dist <- anosim(data[,-ncol(data)],data[,group[1]],permutations = 999,distance = 'bray')
dist
# ANOSIM statistic R: 0.2664 
# Significance: 0.001 

#Site
dist <- anosim(data[,-ncol(data)],data[,group[2]],permutations = 999,distance = 'bray')
dist
# ANOSIM statistic R: 0.04492 
# Significance: 0.002

r <- round(dist$statistic,3)
pval <- dist$signif
plot(dist)

PCoA <- function(data, pos.x, pox.y, group,label,r,pval,anno.x,anno.y){
  distance <- vegdist(data,method="bray") 
  pcoa = cmdscale(distance, k=3, eig=T) # k is dimension, 3 is recommended; eig is eigenvalues
  points = as.data.frame(pcoa$points) # get coordinate string, format to dataframme
  colnames(points) = c("PCoA1", "PCoA2", "PCoA3") 
  eig = pcoa$eig
  points = cbind(points,sub_data[,group][match(rownames(points),sub_data$Sample)])
  names(points)[4] <- group
  library(ggpubr)
  ggscatter(points,x="PCoA1", y="PCoA2", size = 0.4,
            color=group[1],ellipse = F,  
            mean.point = TRUE, star.plot = TRUE,   
            ggtheme = theme_minimal(),palette = "png") +
    theme_bw() + 
    theme(panel.grid=element_blank(),
          legend.title = element_blank(),
          legend.position=c(pos.x, pox.y),
          #legend.box.background = element_rect(color="grey", size=0.5),
          legend.box.background=element_blank(),
          legend.text = element_text(size = 9),
          axis.text=element_text(colour='black',size=10))+
    scale_color_discrete(labels=label) +
    annotate('text', label = paste0("ANOSIM, ","R = ",r,", p = ",pval),
            x = anno.x, y = anno.y, size = 3,hjust = 0) +
    labs(x = paste("PCoA 1 (", format(100*eig[1]/sum(eig), digits = 4), "%)",sep = ""), 
         y = paste("PCoA 2 (", format(100*eig[2]/sum(eig), digits = 4), "%)",sep = ""))
  
}


addSmallLegend <- function(myPlot, pointSize = 0.5, textSize = 6, spaceLegend = 0.5) {
  myPlot +
    guides(shape = guide_legend(override.aes = list(size = pointSize)),
           color = guide_legend(override.aes = list(size = pointSize),ncol = 1)) +
    theme(legend.title = element_blank(), 
          legend.text  = element_text(size = textSize),
          legend.background = element_blank(),
          legend.key.size = unit(spaceLegend, "lines"))
}

p.PCoA <- addSmallLegend(PCoA(data3, 0.13,0.11,group[1],label2, r,pval,-0.48,0.45)) #Population
p.PCoA <- addSmallLegend(PCoA(data3, 0.1,0.08,group[2],label, r, pval,-0.48,0.24)) #Site

p.save <- function(group,width,height){
  filename <- paste(paste(group,"ARG",sep = "_"),"PCoA",sep = ".")
  #tif
  ggsave(plot = p.PCoA, filename = paste(filename,"tiff",sep = "."),width = width,height = height)
  #pdf
  ggsave(plot = p.PCoA, filename = paste(filename,"pdf",sep = "."),width = width, height = height)
}

p.save(group[1], 4, 3.5) #Population
p.save(group[2], 4, 3.5) #Site


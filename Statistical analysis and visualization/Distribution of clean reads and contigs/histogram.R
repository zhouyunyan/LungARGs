
data <- read.table('clean.tab',head=F)
names(data) <- c("SampleID","num")
data$label <- data$num/1000000000

min(data$label)
# 0.6924288
max(data$label)
# 23.89296

library(ggplot2)

p <- ggplot(data,aes(x=label,y=..count..)) + 
  geom_histogram(color='black',fill='grey',bins = 50,size=0.2) +
  geom_freqpoly(color='red',bins = 50)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12,color="black"),
        axis.text.y = element_text(size = 12,color="black")) +
  xlim(0,5) +
  labs(x = 'Bases of clean data (Gb)', y = 'Frequency',)

tiff(filename = "clean_distribution.tif",width = 2700,height = 2500,res=600,compression="lzw")
p
dev.off()
pdf(file = "clean_distribution.pdf",width = 4,height = 4)
p
dev.off()

##N50

data2 <- read.table('Sample_N50.tab',head=F)
names(data2) <- c("SampleID","N50")
min(data2$N50)
# 639
max(data2$N50)
# 3742
mean(data2$N50)
# 1626.387

library(ggplot2)
p <- ggplot(data2,aes(x=N50,y=..count..)) + 
  geom_histogram(color='black',fill='grey',bins = 50,size=0.2) +
  geom_freqpoly(color='red',bins = 50)+
  theme_bw()+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(size = 12,color="black"),
        axis.text.y = element_text(size = 12,color="black")) +
  labs(x = 'N50 (bp)', y = 'Frequency',)

tiff(filename = "N50_distribution.tif",width = 2700,height = 2500,res=600,compression="lzw")
p
dev.off()
pdf(file = "N50_distribution.pdf",width = 4,height = 4)
p
dev.off()

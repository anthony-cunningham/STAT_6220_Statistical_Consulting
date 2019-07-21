dendriteData=read.csv("C:/Users/Anthony/OneDrive/Documents/STAT 6220 Assignment 4/DendriteData.csv", header=TRUE)
## Strip out all "NA" Observation

attach(dendriteData)

png(file = "orig_boxplot.png", res = 300)

pdf("orig_boxplot.pdf", width = 8, height = 6, paper = "USr")

boxplot(diff~GroupWithLabels,main="")
abline(h=0,col=4,lwd=1)
polygon(c(4.5,5.5,5.5,4.5),c(-45,-45,25,25),col="#0000ff22")

dev.off()

detach(dendriteData)
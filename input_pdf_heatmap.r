library("matrixstats")
library("CompexHeatmap")

#input
data1 = source("logCPM.rds")
f2<-function(data1){
  for(i in 1:ncol(data1)){
    z_scores = (data1 - rowMeans(data1))/rowSds(as.matrix(data))[row(data1)]  #z_score
  }
  z_scores[is.na(z_scores)]=0
  z<-as.matrix(z_scores)
  return(Heatmap(z[1:10]))   #heatmap
}
f2(data1)

pdf('data1.pdf',width = 10,height = 10)   #to print plot in pdf format
plot(1:5,pch = 20)
dev.off()
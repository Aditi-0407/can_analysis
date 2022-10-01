library("matrixstats")
library("ComplexHeatmap")

c<-read.csv("C:/Users/Dell !/OneDrive/Desktop/can_gene_counts.csv",sep=",",header=T,row.names = 1)
c

function1 = function(c){
  mat<-c
  s<-as.numeric(unlist(mat))
 
   for (i in 1:ncol(x)) {
    mat[,i] = (c[,i]/sum(c[,i]))*1000000
    print(head(mat))
    mat[,i]= log2(mat[,i] +1)
    logfc=log2(mat+1)
   }

data1 = mat
for (i in 1:ncol(data)){
  z_score = (data1 - rowMeans(data1))/rowSds(as.matrix(data1))[row(data1)]
  
}
z_score[is.na(z_score)]=0
z_scores = as.matrix(z_score)

Heatmap(z_scores)
return(Heatmap(z_scores[1:10],))
}

function1<-(c)


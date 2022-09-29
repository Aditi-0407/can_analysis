library("complexheatmap")
library("readxl")
library("circlize")
d<-read.csv("C:/Users/Dell !/OneDrive/Documents/can.csv",row.names=1)
print(d)

#cpm
cpm_matrix = d
for(i in 1:ncol(d)){
  cpm_matrix[,i]=(d[,i]/sum(d[,i]))*1000000
}
cpm_matrix[is.na(cpm_matrix)]=0

#log
logcpm = log2(cpm_matrix+1)
summary(logcpm)

#z-score
z_score = (logcpm-rowMeans(logcpm))/rowSds(as.matrix(logcpm))
row(logcpm)
z_score[is.na(z_score)]=0
print(z_score)

#variance
vargenes = apply(z_scores,1,var)
print(vargenes)

vargenes = sort(vargenes,decreasing = T)
top50 = vargenes[1:50]
pmat = z_score[names(top50),]

#heatmap
library(ComplexHeatmap)
heatmap(pmat)

#

mat = matrix(NA, ncol= 4, nrow= nrow(d))
rownames(mat)= rownames(d)
colnames(mat)= c('grp1','grp2','pval','log2FC')
print(mat)

for(i in 1:nrow(d)){
  vec1 = as.numeric(d[i,1:4])
  vec2 = as.numeric(d[i,5:7])
  res = t.test(vec1, vec2, paired = F, alternative = 'two.sided')
  mat[i,1]= res$estimate[[1]]
  mat[i,2]= res$estimate[[2]]
  mat[i,3]= res$p.value
  mat[1,4]=mat[i,1]- mat[i,2]
  
}

mat= as.data.frame(mat)
num = which(is.nan(mat$pval))
mat[num,'pval']=1

#volcano plot
library("EnhancedVolcano")
EnhancedVolcano(mat,lab = rownames(mat),x='log2FC',y= 'pval')





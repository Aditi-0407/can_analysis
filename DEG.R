library(ComplexHeatmap)
heatmap(m)

#

mat = matrix(NA, ncol= 4, nrow= nrow(file))
rownames(mat)= rownames(file)
colnames(mat)= c('grp1','grp2','pval','log2FC')
print(mat)

for(i in 1:nrow(file)){
  vec1 = as.numeric(file[i,1:4])
  vec2 = as.numeric(file[i,5:7])
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

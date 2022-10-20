install.packages("seurat")
install.packages("dplyr")
install.packages("patchwork")

library("Seurat")
library("dplyr")
library("patchwork")

pbmc.data = Read10X(data.dir = "filtered_gene_bc_matrices/hg19/")
print(pbmc.data)

pbmc = CreateSeuratObject(counts = pbmc.data, min.cells = 3, min.features = 200)
pbmc

pbmc.data[1:50, 1:10]

pbmc[["percent.mt"]] = PercentageFeatureSet(pbmc, pattern = "^MT-")
head(pbmc@meta.data)

VlnPlot(pbmc, features = c("nFeature_RNA", "nCount_RNA", "percent.mt"), ncol = 3)

plot1 = FeatureScatter(pbmc, feature1 = "nCount_RNA", feature2 = "percent.mt")
plot1

pbmc = subset(pbmc, subset = nFeature_RNA > 200 & nFeature_RNA < 2500 & percent.mt < 5)
pbmc

pbmc = NormalizeData(pbmc)
pbmc = FindVariableFeatures(pbmc, selection.method = "vst", nfeatures = 2000)

top10 = head(VariableFeatures(pbmc), 10)
top10

plot1 = VariableFeaturePlot(pbmc)
plot1

plot2 = LabelPoints(plot = plot1, points = top10, repel = TRUE)
plot2

all.genes = rownames(pbmc)
print(all.genes)
pbmc = ScaleData(pbmc, features = all.genes)

pbmc@assays$RNA@scale.data[1:50, 1:5]

pbmc = RunPCA(pbmc, features = VariableFeatures(object = pbmc))
print(pbmc)

DimHeatmap(pbmc, dims = 1:15, cells = 500, balanced = TRUE)
ElbowPlot(pbmc)

pbmc = FindNeighbors(pbmc, dims = 1:10)

pbmc = FindClusters(pbmc, resolution = 0.5)

head(pbmc@meta.data)

pbmc = RunUMAP(pbmc, dims = 1:10)

DimPlot(pbmc, reduction = "umap")

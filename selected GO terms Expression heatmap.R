#Select GOterms to express heatmapping
library("pheatmap")

A9605_HD_RBST_root<-read.csv("9605_HD-RBST_root.csv",header=1,row.names=1,check.names=F)
A9605_HD_RBST_stem<-read.csv("9605_HD-RBST_stem.csv",header=1,row.names=1,check.names=F)
A9605_K326_root<-read.csv("9605_K326_root.csv",header=1,row.names=1,check.names=F)
A9605_K326_stem<-read.csv("9605_K326_stem.csv",header=1,row.names=1,check.names=F)

A9607_HD_RBST_root<-read.csv("9607_HD-RBST_root.csv",header=1,row.names=1,check.names=F)
A9607_HD_RBST_stem<-read.csv("9607_HD-RBST_stem.csv",header=1,row.names=1,check.names=F)
A9607_K326_root<-read.csv("9607_K326_root.csv",header=1,row.names=1,check.names=F)
A9607_K326_stem<-read.csv("9607_K326_stem.csv",header=1,row.names=1,check.names=F)

A9620_HD_RBST_root<-read.csv("9620_HD-RBST_root.csv",header=1,row.names=1,check.names=F)
A9620_HD_RBST_stem<-read.csv("9620_HD-RBST_stem.csv",header=1,row.names=1,check.names=F)
A9620_K326_root<-read.csv("9620_K326_root.csv",header=1,row.names=1,check.names=F)
A9620_K326_stem<-read.csv("9620_K326_stem.csv",header=1,row.names=1,check.names=F)

A9628_HD_RBST_root<-read.csv("9628_HD-RBST_root.csv",header=1,row.names=1,check.names=F)
A9628_HD_RBST_stem<-read.csv("9628_HD-RBST_stem.csv",header=1,row.names=1,check.names=F)
A9628_K326_root<-read.csv("9628_K326_root.csv",header=1,row.names=1,check.names=F)
A9628_K326_stem<-read.csv("9628_K326_stem.csv",header=1,row.names=1,check.names=F)

A9719_HD_RBST_root<-read.csv("9719_HD-RBST_root.csv",header=1,row.names=1,check.names=F)
A9719_HD_RBST_stem<-read.csv("9719_HD-RBST_stem.csv",header=1,row.names=1,check.names=F)
A9719_K326_root<-read.csv("9719_K326_root.csv",header=1,row.names=1,check.names=F)
A9719_K326_stem<-read.csv("9719_K326_stem.csv",header=1,row.names=1,check.names=F)

A43207_HD_RBST_root<-read.csv("43207_HD-RBST_root.csv",header=1,row.names=1,check.names=F)
A43207_HD_RBST_stem<-read.csv("43207_HD-RBST_stem.csv",header=1,row.names=1,check.names=F)
A43207_K326_root<-read.csv("43207_K326_root.csv",header=1,row.names=1,check.names=F)
A43207_K326_stem<-read.csv("43207_K326_stem.csv",header=1,row.names=1,check.names=F)

A50832_HD_RBST_root<-read.csv("50832_HD-RBST_root.csv",header=1,row.names=1,check.names=F)
A50832_HD_RBST_stem<-read.csv("50832_HD-RBST_stem.csv",header=1,row.names=1,check.names=F)
A50832_K326_root<-read.csv("50832_K326_root.csv",header=1,row.names=1,check.names=F)
A50832_K326_stem<-read.csv("50832_K326_stem.csv",header=1,row.names=1,check.names=F)

A51707_HD_RBST_root<-read.csv("51707_HD-RBST_root.csv",header=1,row.names=1,check.names=F)
A51707_HD_RBST_stem<-read.csv("51707_HD-RBST_stem.csv",header=1,row.names=1,check.names=F)
A51707_K326_root<-read.csv("51707_K326_root.csv",header=1,row.names=1,check.names=F)
A51707_K326_stem<-read.csv("51707_K326_stem.csv",header=1,row.names=1,check.names=F)

A98542_HD_RBST_root<-read.csv("98542_HD-RBST_root.csv",header=1,row.names=1,check.names=F)
A98542_HD_RBST_stem<-read.csv("98542_HD-RBST_stem.csv",header=1,row.names=1,check.names=F)
A98542_K326_root<-read.csv("98542_K326_root.csv",header=1,row.names=1,check.names=F)
A98542_K326_stem<-read.csv("98542_K326_stem.csv",header=1,row.names=1,check.names=F)


annotation_col_root =data.frame(Tissue=factor(c(rep("root",5))))
annotation_col_stem =data.frame(Tissue=factor(c(rep("stem",5))))

row.names(annotation_col_root) = colnames(data)
row.names(annotation_col_stem) = colnames(data)

ann_colors_root = list(Tissue=c(root="#78F7BA"))
ann_colors_stem = list(Tissue=c(stem="#FFB445"))

A9605_HD_RBST_root <- A9605_HD_RBST_root[apply(A9605_HD_RBST_root, 1, function(x) sd(x)!=0),]
A9605_HD_RBST_stem <- A9605_HD_RBST_stem[apply(A9605_HD_RBST_stem, 1, function(x) sd(x)!=0),]
A9605_K326_root <- A9605_K326_root[apply(A9605_K326_root, 1, function(x) sd(x)!=0),]
A9605_K326_stem <- A9605_K326_stem[apply(A9605_K326_stem, 1, function(x) sd(x)!=0),]

A9607_HD_RBST_root <- A9607_HD_RBST_root[apply(A9607_HD_RBST_root, 1, function(x) sd(x)!=0),]
A9607_HD_RBST_stem <- A9607_HD_RBST_stem[apply(A9607_HD_RBST_stem, 1, function(x) sd(x)!=0),]
A9607_K326_root <- A9607_K326_root[apply(A9607_K326_root, 1, function(x) sd(x)!=0),]
A9607_K326_stem <- A9607_K326_stem[apply(A9607_K326_stem, 1, function(x) sd(x)!=0),]

A9620_HD_RBST_root <- A9620_HD_RBST_root[apply(A9620_HD_RBST_root, 1, function(x) sd(x)!=0),]
A9620_HD_RBST_stem <- A9620_HD_RBST_stem[apply(A9620_HD_RBST_stem, 1, function(x) sd(x)!=0),]
A9620_K326_root <- A9620_K326_root[apply(A9620_K326_root, 1, function(x) sd(x)!=0),]
A9620_K326_stem <- A9620_K326_stem[apply(A9620_K326_stem, 1, function(x) sd(x)!=0),]

A9628_HD_RBST_root <- A9628_HD_RBST_root[apply(A9628_HD_RBST_root, 1, function(x) sd(x)!=0),]
A9628_HD_RBST_stem <- A9628_HD_RBST_stem[apply(A9628_HD_RBST_stem, 1, function(x) sd(x)!=0),]
A9628_K326_root <- A9628_K326_root[apply(A9628_K326_root, 1, function(x) sd(x)!=0),]
A9628_K326_stem <- A9628_K326_stem[apply(A9628_K326_stem, 1, function(x) sd(x)!=0),]

A9719_HD_RBST_root <- A9719_HD_RBST_root[apply(A9719_HD_RBST_root, 1, function(x) sd(x)!=0),]
A9719_HD_RBST_stem <- A9719_HD_RBST_stem[apply(A9719_HD_RBST_stem, 1, function(x) sd(x)!=0),]
A9719_K326_root <- A9719_K326_root[apply(A9719_K326_root, 1, function(x) sd(x)!=0),]
A9719_K326_stem <- A9719_K326_stem[apply(A9719_K326_stem, 1, function(x) sd(x)!=0),]

A43207_HD_RBST_root <- A43207_HD_RBST_root[apply(A43207_HD_RBST_root, 1, function(x) sd(x)!=0),]
A43207_HD_RBST_stem <- A43207_HD_RBST_stem[apply(A43207_HD_RBST_stem, 1, function(x) sd(x)!=0),]
A43207_K326_root <- A43207_K326_root[apply(A43207_K326_root, 1, function(x) sd(x)!=0),]
A43207_K326_stem <- A43207_K326_stem[apply(A43207_K326_stem, 1, function(x) sd(x)!=0),]

A50832_HD_RBST_root <- A50832_HD_RBST_root[apply(A50832_HD_RBST_root, 1, function(x) sd(x)!=0),]
A50832_HD_RBST_stem <- A50832_HD_RBST_stem[apply(A50832_HD_RBST_stem, 1, function(x) sd(x)!=0),]
A50832_K326_root <- A50832_K326_root[apply(A50832_K326_root, 1, function(x) sd(x)!=0),]
A50832_K326_stem <- A50832_K326_stem[apply(A50832_K326_stem, 1, function(x) sd(x)!=0),]

A51707_HD_RBST_root <- A51707_HD_RBST_root[apply(A51707_HD_RBST_root, 1, function(x) sd(x)!=0),]
A51707_HD_RBST_stem <- A51707_HD_RBST_stem[apply(A51707_HD_RBST_stem, 1, function(x) sd(x)!=0),]
A51707_K326_root <- A51707_K326_root[apply(A51707_K326_root, 1, function(x) sd(x)!=0),]
A51707_K326_stem <- A51707_K326_stem[apply(A51707_K326_stem, 1, function(x) sd(x)!=0),]

A98542_HD_RBST_root <- A98542_HD_RBST_root[apply(A98542_HD_RBST_root, 1, function(x) sd(x)!=0),]
A98542_HD_RBST_stem <- A98542_HD_RBST_stem[apply(A98542_HD_RBST_stem, 1, function(x) sd(x)!=0),]
A98542_K326_root <- A98542_K326_root[apply(A98542_K326_root, 1, function(x) sd(x)!=0),]
A98542_K326_stem <- A98542_K326_stem[apply(A98542_K326_stem, 1, function(x) sd(x)!=0),]

pheatmap(A9605_HD_RBST_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9605_HD_RBST_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9605_K326_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9605_K326_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9607_HD_RBST_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9607_HD_RBST_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9607_K326_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9607_K326_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9620_HD_RBST_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 3,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9620_HD_RBST_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 3,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9620_K326_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 3,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9620_K326_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 3,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9628_HD_RBST_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9628_HD_RBST_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9628_K326_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9628_K326_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9719_HD_RBST_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9719_HD_RBST_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9719_K326_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A9719_K326_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A43207_HD_RBST_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A43207_HD_RBST_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A43207_K326_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A43207_K326_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A50832_HD_RBST_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 3,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A50832_HD_RBST_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 3,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A50832_K326_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 3,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A50832_K326_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 3,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A51707_HD_RBST_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A51707_HD_RBST_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A51707_K326_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A51707_K326_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A98542_HD_RBST_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A98542_HD_RBST_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A98542_K326_root,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)
pheatmap(A98542_K326_stem,
         scale = "row",cluster_cols = FALSE,
         clustering_distance_rows = "correlation",
         treeheight_row = 30,
         cutree_rows =5,
         cellwidth = 20,
         cellheight = 1.2,
         show_rownames = FALSE,
         border="NA",
         angle_col = 45,
         fontsize_col = 12)

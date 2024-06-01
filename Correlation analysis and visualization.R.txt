#Correlation analysis and visualization

library(pheatmap)

Log2(TPM+1)_1<-read.csv("Log2(TPM+1)_1.csv")

Log2(TPM+1)_1_matrix<-as.matrix(Log2(TPM+1)_1)

row.names(Log2(TPM+1)_1_matrix)<-Log2(TPM+1)_1_matrix[,1]

Log2(TPM+1)_1_matrix<-Log2(TPM+1)_1_matrix[,-1]

bk<-c(seq(-3,2,by=0.01))

Log2(TPM+1)_1_matrix_numeric=apply(Log2(TPM+1)_1_matrix,2,as.numeric)

Log2(TPM+1)_1_matrix_numeric_cor<-cor(Log2(TPM+1)_1_matrix_numeric)

write.table(Log2(TPM+1)_1_matrix_numeric_cor, "Log2(TPM+1)_1_cor.csv", sep=":") 

annotation_col = data.frame(Type=factor(c(rep("root",60),rep("stem",60))),
                            Sample = factor(c(rep("HD_S",15),rep("K326_S",15),rep("K326_R",15),rep("RBST_R",15))))

ann_colors <- list(Type = c(root = "orange", stem = "red"))

row.names(annotation_col) = colnames(Log2(TPM+1)_1_matrix_numeric_cor)

pheatmap(Log2(TPM+1)_1_matrix_numeric_cor,
         cluster_rows = F, 
         scale = "none",
         border = FALSE ,
         fontsize = 5,
         fontfamily= "Times New Roman",
         cluster_cols = FALSE, 
         clustering_distance_rows = "correlation",
         main="Correlation of time of infestation(HD-K326-RBST)-1",
         annotation_col = annotation_col,
         annotation_colors = ann_colors,
         color = colorRampPalette(c("navy","white","firebrick" ))(length(bk)))

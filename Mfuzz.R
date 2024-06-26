#Time series analysis
install.packages('BiocManager')
BiocManager::install('Mfuzz')
library(Mfuzz)
library(readxl)
HD_root=read.csv("HD-root.csv")
K326S_root=read.csv("K326S-root.csv")
K326R_root=read.csv("K326R-root.csv")
RBST_root=read.csv("RBST-root.csv")
HD_stem=read.csv("HD-stem.csv")
K326S_stem=read.csv("K326S-stem.csv")
K326R_stem=read.csv("K326R-stem.csv")
RBST_stem=read.csv("RBST-stem.csv")

rownames(HD_root)<-HD_root[,1]
rownames(K326S_root)<-K326S_root[,1]
rownames(K326R_root)<-K326R_root[,1]
rownames(RBST_root)<-RBST_root[,1]
rownames(HD_stem)<-HD_stem[,1]
rownames(K326S_stem)<-K326S_stem[,1]
rownames(K326R_stem)<-K326R_stem[,1]
rownames(RBST_stem)<-RBST_stem[,1]
HD_root<-HD_root[,-1]
K326S_root<-K326S_root[,-1]
K326R_root<-K326R_root[,-1]
RBST_root<-RBST_root[,-1]
HD_stem<-HD_stem[,-1]
K326S_stem<-K326S_stem[,-1]
K326R_stem<-K326R_stem[,-1]
RBST_stem<-RBST_stem[,-1]

##Convert format
HD_root_tpm <- data.matrix(HD_root)
K326S_root_tpm <- data.matrix(K326S_root)
K326R_root_tpm <- data.matrix(K326R_root)
RBST_root_tpm <- data.matrix(RBST_root)
HD_stem_tpm <- data.matrix(HD_stem)
K326S_stem_tpm <- data.matrix(K326S_stem)
K326R_stem_tpm <- data.matrix(K326R_stem)
RBST_stem_tpm <- data.matrix(RBST_stem)

HD_root_eset <- new("ExpressionSet",exprs = HD_root_tpm)
K326S_root_eset <- new("ExpressionSet",exprs = K326S_root_tpm)
K326R_root_eset <- new("ExpressionSet",exprs = K326R_root_tpm)
RBST_root_eset <- new("ExpressionSet",exprs = RBST_root_tpm)
HD_stem_eset <- new("ExpressionSet",exprs = HD_stem_tpm)
K326S_stem_eset <- new("ExpressionSet",exprs = K326S_stem_tpm)
K326R_stem_eset <- new("ExpressionSet",exprs = K326R_stem_tpm)
RBST_stem_eset <- new("ExpressionSet",exprs = RBST_stem_tpm)

## Filter out more than 25% of genes that are missing
HD_root.r <- filter.NA(HD_root_eset, thres=0.25) 
K326S_root.r <- filter.NA(K326S_root_eset, thres=0.25) 
K326R_root.r <- filter.NA(K326R_root_eset, thres=0.25) 
RBST_root.r <- filter.NA(RBST_root_eset, thres=0.25) 
HD_stem.r <- filter.NA(HD_stem_eset, thres=0.25) 
K326S_stem.r <- filter.NA(K326S_stem_eset, thres=0.25) 
K326R_stem.r <- filter.NA(K326R_stem_eset, thres=0.25) 
RBST_stem.r <- filter.NA(RBST_stem_eset, thres=0.25) 

## knn/wknn Fill in the gaps
HD_root.f <- fill.NA(HD_root.r,mode="knn")
K326S_root.f <- fill.NA(K326S_root.r,mode="knn")
K326R_root.f <- fill.NA(K326R_root.r,mode="knn")
RBST_root.f <- fill.NA(RBST_root.r,mode="knn")
HD_stem.f <- fill.NA(HD_stem.r,mode="knn")
K326S_stem.f <- fill.NA(K326S_stem.r,mode="knn")
K326R_stem.f <- fill.NA(K326R_stem.r,mode="knn")
RBST_stem.f <- fill.NA(RBST_stem.r,mode="knn")


HD_root.f <- fill.NA(HD_root.r,mode="wknn")
K326S_root.f <- fill.NA(K326S_root.r,mode="wknn")
K326R_root.f <- fill.NA(K326R_root.r,mode="wknn")
RBST_root.f <- fill.NA(RBST_root.r,mode="wknn")
HD_stem.f <- fill.NA(HD_stem.r,mode="wknn")
K326S_stem.f <- fill.NA(K326S_stem.r,mode="wknn")
K326R_stem.f <- fill.NA(K326R_stem.r,mode="wknn")
RBST_stem.f <- fill.NA(RBST_stem.r,mode="wknn")

## Filter genes with a standard deviation of 0
HD_root_tmp <- filter.std(HD_root.f,min.std=0)
K326S_root_tmp <- filter.std(K326S_root.f,min.std=0) 
K326R_root_tmp <- filter.std(K326R_root.f,min.std=0) 
RBST_root_tmp <- filter.std(RBST_root.f,min.std=0) 
HD_stem_tmp <- filter.std(HD_stem.f,min.std=0) 
K326S_stem_tmp <- filter.std(K326S_stem.f,min.std=0) 
K326R_stem_tmp <- filter.std(K326R_stem.f,min.std=0) 
RBST_stem_tmp <- filter.std(RBST_stem.f,min.std=0) 

## standardization
HD_root.s <- standardise(HD_root_tmp)
K326S_root.s <- standardise(K326S_root_tmp)
K326R_root.s <- standardise(K326R_root_tmp)
RBST_root.s <- standardise(RBST_root_tmp)
HD_stem.s <- standardise(HD_stem_tmp)
K326S_stem.s <- standardise(K326S_stem_tmp)
K326R_stem.s <- standardise(K326R_stem_tmp)
RBST_stem.s <- standardise(RBST_stem_tmp)

## Number of clusters
c <- 100
m <- mestimate(HD_root_eset)

## Calculate the optimal m-value
m1 <- mestimate(HD_root.s)
m2 <- mestimate(K326S_root.s)
m3 <- mestimate(K326R_root.s)
m4 <- mestimate(RBST_root.s)
m5 <- mestimate(HD_stem.s)
m6 <- mestimate(K326S_stem.s)
m7 <- mestimate(K326R_stem.s)
m8 <- mestimate(RBST_stem.s)

## clustering
cl <- mfuzz(HD_root.s, c = c, m = m1)
c2 <- mfuzz(K326S_root.s, c = c, m = m2)
c3 <- mfuzz(K326R_root.s, c = c, m = m3)
c4 <- mfuzz(RBST_root.s, c = c, m = m4)
c5 <- mfuzz(HD_stem.s, c = c, m = m5)
c6 <- mfuzz(K326S_stem.s, c = c, m = m6)
c7 <- mfuzz(K326R_stem.s, c = c, m = m7)
c8 <- mfuzz(RBST_stem.s, c = c, m = m8)

## View the number of genes in each class
cl$size
c2$size
c3$size
c4$size
c5$size
c6$size
c7$size
c8$size

## View the gene IDs for each class
cl$cluster[cl$cluster == 1]
c2$cluster[c2$cluster == 1]
c3$cluster[c3$cluster == 1]
c4$cluster[c4$cluster == 1]
c5$cluster[c5$cluster == 1]
c6$cluster[c6$cluster == 1]
c7$cluster[c7$cluster == 1]
c8$cluster[c8$cluster == 1]

## Output gene ID
write.table(cl$cluster,"HD_root_output.txt",quote=F,row.names=T,col.names=F,sep="\t")
write.table(c2$cluster,"K326S_root_output.txt",quote=F,row.names=T,col.names=F,sep="\t")
write.table(c3$cluster,"K326R_root_output.txt",quote=F,row.names=T,col.names=F,sep="\t")
write.table(c4$cluster,"RBST_root_output.txt",quote=F,row.names=T,col.names=F,sep="\t")
write.table(c5$cluster,"HD_stem_output.txt",quote=F,row.names=T,col.names=F,sep="\t")
write.table(c6$cluster,"K326S_stem_output.txt",quote=F,row.names=T,col.names=F,sep="\t")
write.table(c7$cluster,"K326R_stem_output.txt",quote=F,row.names=T,col.names=F,sep="\t")
write.table(c8$cluster,"RBST_stem_output.txt",quote=F,row.names=T,col.names=F,sep="\t")

## Draw a line chart
color.2 <- colorRampPalette(rev(c("#ff0000", "Yellow", "OliveDrab1")))(1000)
mfuzz.plot(HD_root.s,cl,mfrow=c(2,3),new.window= FALSE,colo = color.2,time.labels=c("0","12","24","48","72"))
mfuzz.plot(K326S_root.s,c2,mfrow=c(2,3),new.window= FALSE,colo = color.2,time.labels=c("0","12","24","48","72"))
mfuzz.plot(K326R_root.s,c3,mfrow=c(2,3),new.window= FALSE,colo = color.2,time.labels=c("0","12","24","48","72"))
mfuzz.plot(RBST_root.s,c4,mfrow=c(2,3),new.window= FALSE,colo = color.2,time.labels=c("0","12","24","48","72"))
mfuzz.plot(HD_stem.s,c5,mfrow=c(2,3),new.window= FALSE,colo = color.2,time.labels=c("0","12","24","48","72"))
mfuzz.plot(K326S_stem.s,c6,mfrow=c(2,3),new.window= FALSE,colo = color.2,time.labels=c("0","12","24","48","72"))
mfuzz.plot(K326R_stem.s,c7,mfrow=c(2,3),new.window= FALSE,colo = color.2,time.labels=c("0","12","24","48","72"))
mfuzz.plot(RBST_stem.s,c8,mfrow=c(2,3),new.window= FALSE,colo = color.2,time.labels=c("0","12","24","48","72"))
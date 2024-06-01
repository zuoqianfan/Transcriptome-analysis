#TPM standardization

data_non <- read.csv("rawdata.csv",row.names=1)

data_genelenth <-read.csv("gene_length.csv")

data_reads <- read.csv("gene_reads.csv")

data_non_M<-as.data.frame(data_non)

data_non_numeric=apply(data_non_M,2,as.numeric)

data_RPK = data_non_numeric/data_genelenth$length

data_tpm = t(t(data_RPK)/colSums(data_reads)) * 10^9

data_tpm_1<-data_tpm+1

Log2data_tpm_1 <- log2(data_tpm_1)

write.table(data_tpm, "TPM.csv", sep=":") 

write.table(Log2data_tpm_1, "Log2(TPM+1).csv") 
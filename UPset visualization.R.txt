#UPset visualization

install.packages("UpSetR")
library(UpSetR)


UPset_data_2<-read.csv("UPset_filter.csv")
upset(UPset_data_2,
      nsets = 4,
      number.angles = 0,
      point.size = 3,
      line.size = 1,
      mainbar.y.label = "Sample intersection size",
      sets.x.label = "Sample size",
      sets.bar.color = c("#998273","#139177","#ed9e08","#f56f08"))
# Draw density scatter plot
library(ggplot2)
install.packages("ggpointdensity")
library(ggpointdensity) 
library(viridis)
library(dplyr)
library(cowplot)
library(showtext)

data2<-read.csv("Log2TPM_HT_2_ggplot.csv")
data1<-read.csv("Log2TPM_HT_1_ggplot.csv")
data23<-read.csv("Log2HD_RBST_TPM_1_2.csv")
data13<-read.csv("Log2HD_RBST_TPM_1_1.csv")
data_HD_2<-read.csv("HD_ggplot_Log2TPM_1_2.csv")
data_P3R_2<-read.csv("P3R_ggplot_Log2TPM_1_2.csv")
data_P4R_2<-read.csv("P4R_ggplot_Log2TPM_1_2.csv")
data_RBST_2<-read.csv("RBST_ggplot_Log2TPM_1_2.csv")
data_HD_test<-read.csv("D:\\desktop\\graph_HD_0.2.csv")

data22=2^data2
data11=2^data1
data232=2^data23
data132=2^data13
data_HD_22=2^data_HD_2
data_P3R_22=2^data_P3R_2
data_P4R_22=2^data_P4R_2
data_RBST_22=2^data_RBST_2


max_2= apply(data2,2,max)
max_1= apply(data1,2,max)
max_22=apply(data22,2,max)
max_11=apply(data11,2,max)
max_232=max(data232)
max_132=max(data132)
max_HD_22=max(data_HD_22)
max_P3R_22=max(data_P3R_22)
max_P4R_22=max(data_P4R_22)
max_RBST_22=max(data_RBST_22)

min_2= apply(data2,2,min)
min_1= apply(data1,2,min)
min_22=apply(data22,2,min)
min_11=apply(data11,2,min)
min_232=min(data232)
min_132=min(data132)
min_HD_22=min(data_HD_22)
min_P3R_22=min(data_P3R_22)
min_P4R_22=min(data_P4R_22)
min_RBST_22=min(data_RBST_22)

y=2/(max_2-min_2)
y_22=2/(max_22-min_22)
m=2/(max_1-min_1)
m_11=2/(max_11-min_11)
y_232=2/(max_232-min_232)
y_132=2/(max_132-min_132)
y_HD_22=2/(max_HD_22-min_HD_22)
y_P3R_22=2/(max_P3R_22-min_P3R_22)
y_P4R_22=2/(max_P4R_22-min_P4R_22)
y_RBST_22=2/(max_RBST_22-min_RBST_22)

x=(t(t(data2)-min_2))
x_22=(t(t(data22)-min_22))
n=(t(t(data1)-min_1))
n_11=(t(t(data11)-min_11))
x_232=data232-min_232
x_132=data132-min_132
x_HD_22=data_HD_22-min_HD_22
x_P3R_22=data_P3R_22-min_P3R_22
x_P4R_22=data_P4R_22-min_P4R_22
x_RBST_22=data_RBST_22-min_RBST_22

ncc_2=-1+t(t(x)*y)        
ncc_1=-1+t(t(n)*m)
ncc_22=-1+t(t(x_22)*y_22)
ncc_11=-1+t(t(n_11)*m_11)
ncc_232=-1+(x_232*y_232)
ncc_132=-1+(x_132*y_132)
ncc_HD_22=-1+(x_HD_22*y_HD_22)
ncc_P3R_22=-1+(x_P3R_22*y_P3R_22)
ncc_P4R_22=-1+(x_P4R_22*y_P4R_22)
ncc_RBST_22=-1+(x_RBST_22*y_RBST_22)

write.table(ncc_2, "ncc_2.csv", sep=":") 
write.table(ncc_22, "ncc_22_raw.csv", sep=":") 
write.table(ncc_1, "ncc_1.csv", sep=":") 
write.table(ncc_11, "ncc_11_raw.csv", sep=":") 
write.table(ncc_232, "ncc_232_raw.csv", sep=":") 
write.table(ncc_132, "ncc_132_raw.csv", sep=":") 
write.table(ncc_HD_22, "ncc_HD_22_raw.csv", sep=":") 
write.table(ncc_P3R_22, "ncc_P3R_22_raw.csv", sep=":") 
write.table(ncc_P4R_22, "ncc_P4R_22_raw.csv", sep=":") 
write.table(ncc_RBST_22, "ncc_RBST_22_raw.csv", sep=":") 


plot_ncc_2<-read.csv("ncc_2.csv")
plot_ncc_22<-read.csv("ncc_22_raw.csv")
plot_ncc_1<-read.csv("ncc_1.csv")
plot_ncc_11<-read.csv("ncc_11_raw.csv")
plot_ncc_232<-read.csv("ncc_232_raw.csv")
plot_ncc_132<-read.csv("ncc_132_raw.csv")
plot_ncc_HD_22<-read.csv("ncc_HD_22_raw.csv")
plot_ncc_P3R_22<-read.csv("ncc_P3R_22_raw.csv")
plot_ncc_P4R_22<-read.csv("ncc_P4R_22_raw.csv")
plot_ncc_RBST_22<-read.csv("ncc_RBST_22_raw.csv")
plot_ncc_ALL_22<-read.csv("ncc_ALL_22_raw.csv")

S_R_2<-read.csv("K326_S_R_2.csv")
K326_S_root_shoot_2<-read.csv("K326_S_root_shoot_2.csv")
K326_R_root_shoot_2<-read.csv("K326_R_root_shoot_2.csv")
S_R_1<-read.csv("K326_S_R_1.csv")
K326_S_root_shoot_1<-read.csv("K326_S_root_shoot_1.csv")
K326_R_root_shoot_1<-read.csv("K326_R_root_shoot_1.csv")
K326_R_all_root_2<-read.csv("K326_R_all_root_amplitude_2.csv")
K326_R_all_shoot_2<-read.csv("K326_R_all_shoot_amplitude_2.csv")
K326_R_all_root_1<-read.csv("K326_R_all_root_amplitude_1.csv")
K326_R_all_shoot_1<-read.csv("K326_R_all_shoot_amplitude_1.csv")
K326_S_all_root_2<-read.csv("K326_S_all_root_amplitude_2.csv")
K326_S_all_shoot_2<-read.csv("K326_S_all_shoot_amplitude_2.csv")
K326_S_all_root_1<-read.csv("K326_S_all_root_amplitude_1.csv")
K326_S_all_shoot_1<-read.csv("K326_S_all_shoot_amplitude_1.csv")
HD_AM_2<-read.csv("HD_AM_2.csv")
K326S_AM_2<-read.csv("K326S_AM_2.csv")
K326R_AM_2<-read.csv("K326R_AM_2.csv")
RBST_AM_2<-read.csv("RBST_AM_2.csv")
correlation<-read.csv("correlation_cor_2.csv")
K326SR_root_2<-read.csv("K326SR_root_2.csv")
K326SR_shoot_2<-read.csv("K326SR_shoot_2.csv")
HDRBST_root_2<-read.csv("HDRBST_root_2.csv")
HDRBST_shoot_2<-read.csv("HDRBST_shoot_2.csv")
HDRBST_all_2<-read.csv("HDRBST_all_2.csv")
time_all<-read.csv("time_all.csv")
time_R_root<-read.csv("time_R_root.csv")
time_R_shoot<-read.csv("time_R_shoot.csv")
time_HR_root<-read.csv("time_HR_root.csv")
time_HR_shoot<-read.csv("time_HR_shoot.csv")
time_k326_root<-read.csv("time_k326_root.csv")
time_k326_shoot<-read.csv("time_k326_shoot.csv")
time_S_root<-read.csv("time_S_root.csv")
time_S_shoot<-read.csv("time_S_shoot.csv")
time_K326_root_1<-read.csv("time_K326_root.csv")
time_K326_shoot_1<-read.csv("time_K326_shoot.csv")
time_HR_root_1<-read.csv("time_HR_root.csv")
time_HR_shoot_1<-read.csv("time_HR_shoot.csv")
time_S_root_1<-read.csv("time_S_root.csv")
time_S_shoot_1<-read.csv("time_S_shoot.csv")
time_R_root_1<-read.csv("time_R_root.csv")
time_R_shoot_1<-read.csv("time_R_shoot.csv")


g <- ggplot(plot_ncc_2, aes(x = scafold, y = NCC, color=time))
g + geom_point(color = "firebrick", shape = "diamond", size = 2)+
  labs(x = "Sample", y = "Normalized correlation",
       title = "Correlation",
       subtitle = "Normalized correlation between samples")+
  theme(axis.title = element_text(size = 15, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 8))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_22, aes(x = sample, y = NCC, color=time))
g + geom_point(color = "black", size = 2)+
  labs(x = "Sample", y = "TPM",
       title = "Correlation",
       subtitle = "Normalized correlation between samples")+
  theme(axis.title = element_text(size = 15, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 8))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_1, aes(x = sample, y = NCC, color=time))
g + geom_point(color = "firebrick", shape = "diamond", size = 2)+
  labs(x = "Sample", y = "Normalized correlation",
       title = "Correlation",
       subtitle = "Normalized correlation between samples")+
  theme(axis.title = element_text(size = 15, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 8))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_11, aes(x = sample, y = NCC, color=time))
g + geom_point(color = "black", size = 2)+
  labs(x = "Sample", y = "TPM",
       title = "Correlation",
       subtitle = "Normalized correlation between samples")+
  theme(axis.title = element_text(size = 15, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 8))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_232, aes(x = sample, y = NCC))
g + geom_point(color = "black", size = 1,alpha = 0.3)+
  labs(x = "Sample", y = "Normalized TPM",
       title = "Expression in the sample",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 3))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_132, aes(x = sample, y = NCC))
g + geom_point(color = "black", size = 1,alpha = 0.3)+
  labs(x = "Sample", y = "Normalized TPM",
       title = "Expression in the sample",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 3))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_HD_22, aes(x = sample, y = NCC))
g + geom_point(color = "black", size = 1,alpha = 0.3)+
  labs(x = "Sample", y = "Normalized TPM",
       title = "Expression in the sample",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 3))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_P3R_22, aes(x = sample, y = NCC))
g + geom_point(color = "black", size = 1,alpha = 0.3)+
  labs(x = "Sample", y = "Normalized TPM",
       title = "Expression in the sample",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 3))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_P4R_22, aes(x = sample, y = NCC))
g + geom_point(color = "black", size = 1,alpha = 0.3)+
  labs(x = "Sample", y = "Normalized TPM",
       title = "Expression in the sample",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 3))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_RBST_22, aes(x = sample, y = NCC))
g + geom_point(color = "black", size = 1,alpha = 0.3)+
  labs(x = "Sample", y = "Normalized TPM",
       title = "Expression in the sample",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 3))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(plot_ncc_ALL_22, aes(x = sample, y = NCC))
g + geom_point(color = "black", size = 1,alpha = 0.3)+
  labs(x = "Sample", y = "Normalized TPM",
       title = "Expression in the sample",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  ylim(c(-1,1))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 3))+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(S_R_2, aes(x = S_AM, y = R_AM))
g + geom_point(color = "black", size = 0.8,alpha = 0.3)+
  labs(x = "K326_S", y = "K326_R",
       title = "Amplitude of K326_S_R_2",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(S_R_1, aes(x = S_AM, y = R_AM))
g + geom_point(color = "black", size = 0.8,alpha = 0.3)+
  labs(x = "K326_S", y = "K326_R",
       title = "Amplitude of K326_S_R_1",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(K326_S_root_shoot_2, aes(x = root_AM, y = shoot_AM))
g + geom_point(color = "black", size = 1,alpha = 0.5)+
  labs(x = "K326_S_Root", y = "K326_S_Shoot",
       title = "Amplitude of K326_S__root_shoot_2",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))

#调色
p <- ggplot(K326_S_root_shoot, aes(root_AM, shoot_AM)) + geom_point(aes(colour = shoot_AM))
p + scale_color_gradient2(low = "red", mid = "white", high = "blue")


g <- ggplot(K326_R_root_shoot_2, aes(x = root_AM, y = shoot_AM))
g + geom_point(color = "black", size = 1,alpha = 0.5)+
  labs(x = "K326_R_root", y = "K326_R_shoot",
       title = "Amplitude of K326_R__root_shoot_2",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(K326_S_root_shoot_1, aes(x = root_AM, y = shoot_AM))
g + geom_point(color = "black", size = 1,alpha = 0.5)+
  labs(x = "K326_S_root", y = "K326_S_shoot",
       title = "Amplitude of K326_S__root_shoot_1",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
g <- ggplot(K326_R_root_shoot_1, aes(x = root_AM, y = shoot_AM))
g + geom_point(color = "black", size = 1,alpha = 0.5)+
  labs(x = "K326_R_root", y = "K326_R_shoot",
       title = "Amplitude of K326_R__root_shoot_1",)+
  theme(axis.title = element_text(size = 12, color = "black"))+
  theme(panel.grid = element_blank())+
  theme(panel.background = NULL)+
  theme(plot.title = element_text(face = "bold",
                                  margin = margin(10, 0, 10, 0),
                                  size = 14))
ggplot(S_R_2, aes(x=line)) +
  geom_histogram(bins=50,color="black",fill="white")+
  labs(title="Amplitude of Log2(TPM+1) in K326_S_R_2",
       caption = "caption",
       x = 'Amplitude of Log2(tpm+1)',
       y = 'No. of scafolds')
ggplot(S_R_1, aes(x=line)) +
  geom_histogram(bins=50,color="black",fill="white")+
  labs(title="Amplitude of Log2(TPM+1) in K326_S_R_1",
       caption = "caption",
       x = 'Amplitude of Log2(tpm+1)',
       y = 'No. of scafolds')
ggplot(HD_AM_2, aes(x=line)) +
  geom_histogram(bins=50,color="black",fill="white")+
  labs(title="Amplitude of Log2(TPM+1) in HD_2",
       caption = "caption",
       x = 'Amplitude of Log2(tpm+1)',
       y = 'No. of scafolds')
ggplot(K326S_AM_2, aes(x=line)) +
  geom_histogram(bins=50,color="black",fill="white")+
  labs(title="Amplitude of Log2(TPM+1) in K326_S_2",
       caption = "caption",
       x = 'Amplitude of Log2(tpm+1)',
       y = 'No. of scafolds')
ggplot(K326R_AM_2, aes(x=line)) +
  geom_histogram(bins=50,color="black",fill="white")+
  labs(title="Amplitude of Log2(TPM+1) in K326_R_2",
       caption = "caption",
       x = 'Amplitude of Log2(tpm+1)',
       y = 'No. of scafolds')
ggplot(RBST_AM_2, aes(x=line)) +
  geom_histogram(bins=50,color="black",fill="white")+
  labs(title="Amplitude of Log2(TPM+1) in RBST_2",
       caption = "caption",
       x = 'Amplitude of Log2(tpm+1)',
       y = 'No. of scafolds')
ggplot(correlation, aes(x=line)) +
  geom_histogram(bins=30,color="black",fill="white",binwidth = 0.05)+
  labs(title="Absolute correlations with sample (2)",
       caption = "caption",
       x = 'Absolute correlation',
       y = 'No. of scafolds')
ggplot(data = K326_S_root_shoot_1, mapping = aes(x =root_AM, y = shoot_AM)) +
               geom_bin2d(bins = 80) +
               scale_fill_viridis() +
               labs(tag = "") +
               labs(x = "K326_S_root", y = "K326_S_shoot",
                    title = "Amplitude of K326_S__root_shoot_1",)
               theme_classic()
ggplot(data = K326_R_root_shoot_1, mapping = aes(x =root_AM, y = shoot_AM)) +
               geom_bin2d(bins = 80) +
               scale_fill_viridis() +
               labs(tag = "") +
               labs(x = "K326_R_root", y = "K326_R_shoot",
                  title = "Amplitude of K326_R__root_shoot_1",)
               theme_classic()

ggplot(data = S_R_1, mapping = aes(x =S_AM, y = R_AM)) +
                 geom_bin2d(bins = 80) +
                 scale_fill_viridis() +
                 labs(tag = "") +
                 labs(x = "K326_S", y = "K326_R",
                      title = "Amplitude of K326_S_R_1",)
               theme_classic()
ggplot(data = K326_R_all_root_2, mapping = aes(x =all_AM, y = root_AM)) +
      geom_bin2d(bins = 80) +
      scale_fill_viridis() +
      labs(tag = "") +
      labs(x = "K326_R_all", y = "K326_R_root",
      title = "Amplitude of K326_R_root_all_2",)
               theme_classic()
ggplot(data = K326_R_all_shoot_2, mapping = aes(x =all_AM, y = shoot_AM)) +
      geom_bin2d(bins = 80) +
      scale_fill_viridis() +
      labs(tag = "") +
      labs(x = "K326_R_all", y = "K326_R_shoot",
      title = "Amplitude of K326_R_shoot_all_2",)
               theme_classic() 
ggplot(data = K326_R_all_root_1, mapping = aes(x =all_AM, y = root_AM)) +
                 geom_bin2d(bins = 80) +
                 scale_fill_viridis() +
                 labs(tag = "") +
                 labs(x = "K326_R_all", y = "K326_R_root",
                      title = "Amplitude of K326_R_shoot_all_2",)+
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1) 
ggplot(data = K326SR_root_2, mapping = aes(x =K326_S_root_AM, y = K326_R_root_AM)) +
  geom_bin2d(bins = 80) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "K326_S_root", y = "K326_R_root",
       title = "Amplitude of K326_S_R_root_2",)+
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1)
ggplot(data = K326SR_shoot_2, mapping = aes(x =K326_S_shoot_AM, y = K326_R_shoot_AM)) +
  geom_bin2d(bins = 80) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "K326_S_shoot", y = "K326_R_shoot",
       title = "Amplitude of K326_S_R_shoot_2",)+
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1)
ggplot(data = HDRBST_root_2, mapping = aes(x =HD_root_2, y = RBST_root_2)) +
  geom_bin2d(bins = 80) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD_root", y = "RBST_root",
       title = "Amplitude of HD_RBST_root_2",)+
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1)
ggplot(data = HDRBST_shoot_2, mapping = aes(x =HD_shoot_2, y = RBST_shoot_2)) +
  geom_bin2d(bins = 80) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD_shoot", y = "RBST_shoot",
       title = "Amplitude of HD_RBST_shoot_2",)+
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1)
ggplot(data = HDRBST_all_2, mapping = aes(x =HD_all_2, y = RBST_all_2)) +
  geom_bin2d(bins = 80) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD_all", y = "RBST_all",
       title = "Amplitude of HD_RBST_all_2",)+
  geom_hline(yintercept=1) +
  geom_vline(xintercept=1)
ggplot(data = time_all, mapping = aes(x =S, y =R)) +
  geom_bin2d(bins = 800) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "S", y = "R",title = "consistency")
ggplot(data = time_R_root, mapping = aes(x =K326, y =RBST)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "K326_R", y = "RBST",title = "consistency of resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_R_shoot, mapping = aes(x =K326, y =RBST)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "K326_R", y = "RBST",title = "consistency of resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_HR_root, mapping = aes(x =HD, y =RBST)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD_S", y = "RBST_R",title = "consistency of susceptible")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_HR_shoot, mapping = aes(x =HD, y =RBST)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD_S", y = "RBST_R",title = "consistency of susceptible")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_k326_root, mapping = aes(x =S, y =R)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "k326_S", y = "k326_R",title = "consistency of susceptible and resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_k326_shoot, mapping = aes(x =S, y =R)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "k326_S", y = "k326_R",title = "consistency of susceptible and resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_S_root, mapping = aes(x =HD, y =S)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD_S", y = "k326_S",title = "consistency of susceptible")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_S_shoot, mapping = aes(x =HD, y =S)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD_S", y = "k326_S",title = "consistency of susceptible")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_K326_root_1, mapping = aes(x =S, y =R)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "k326_S", y = "k326_R",title = "consistency of susceptible and resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_K326_shoot_1, mapping = aes(x =S, y =R)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "k326_S", y = "k326_R",title = "consistency of susceptible and resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_HR_root_1, mapping = aes(x =HD, y =RBST)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD", y = "RBST",title = "consistency of susceptible and resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_HR_shoot_1, mapping = aes(x =HD, y =RBST)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD", y = "RBST",title = "consistency of susceptible and resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_S_root_1, mapping = aes(x =HD, y =K326)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD", y = "K326_S",title = "consistency of susceptible")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_S_shoot_1, mapping = aes(x =HD, y =K326)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "HD", y = "K326_S",title = "consistency of susceptible")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_R_root_1, mapping = aes(x =K326, y =RBST)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "K326_R", y = "RBST",title = "consistency of resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank())
ggplot(data = time_R_shoot_1, mapping = aes(x =K326, y =RBST)) +
  geom_bin2d(bins = 5) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "K326_R", y = "RBST",title = "consistency of resistance")+
  theme(axis.text.y = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks = element_blank(),
        panel.grid.major =element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.title = element_text(family = Fon))
windowsFonts()
Fon <- 'serif'
ggplot(data = data_HD_test, mapping = aes(x =root, y = shoot)) +
  geom_bin2d(bins = 30) +
  scale_fill_viridis() +
  labs(tag = "") +
  labs(x = "root", y = "shoot",
       title = "Amplitude of HD_root_shoot_1",)
theme_classic()

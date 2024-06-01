#Amplitude histogram
install.packages('ggplot2')
library(ggplot2)

Amplitude_count<-read.csv("filter_count.csv")

ggplot(Amplitude_count, aes(x=line)) +
  geom_histogram(bins=50,color="black",fill="white")+
  labs(title="Amplitude distribution of 4 varieties ",
       caption = "caption",
       x = 'Amplitude of Log2(Tpm+1)',
       y = 'No. of scafolds')
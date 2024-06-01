#Venn visualization

install.packages("VennDiagram")
library(VennDiagram)
install.packages("cowplot")
library(cowplot)

data<-read.csv("Venn_data.csv")

data_list<-list(HD=data$HD,
                K326S=data$K326S,
                K326R=data$K326R,
                RBST=data$RBST)

Venn <- venn.diagram(data_list, filename = NULL, 
                   #height = 450, 
                   #width = 450,
                   #resolution =300, 
                   #imagetype="png", 
                   col="black",
                   fill=c("#139177","#998273","#ed9e08","#f56f08"),
                   alpha = 0.50, 
                   cex=2, 
                   cat.cex=2,
                   print.mode=c("raw","percent"))
cowplot::plot_grid(Venn)
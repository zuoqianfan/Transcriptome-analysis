#GO and KEGG enrichment plot
library(ggplot2)
goinput <- read.table("enrichment.txt",header = T,sep = "\t")
p = ggplot(goinput,aes(P_value,GO_Name))
p=p + geom_point(aes(size=HitsGenesCountsInSelectedSet,color=-1*log10(P_value)))
pr = p+scale_color_gradient(low="green",high = "red")
pr = pr+labs(color=expression(-log[10](P_value)),size="HitsGenesCountsInSelectedSet",  
             x="P_value",y="Pathway name",title="Pathway enrichment")
pr + theme_bw()
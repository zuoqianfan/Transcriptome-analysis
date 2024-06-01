library(ggalluvial)
d<-read.table("data_2_draw_stem.txt",header = TRUE,sep="\t", comment.char = "")
data_wide <- data.frame(d)
data_wide$K326R.stem=factor(data_wide$K326R.stem,levels = c('K326R_1','K326R_2','K326R_3','K326R_4','K326R_5','K326R_6'))
ggplot(data = data_wide,aes(axis1 = K326R.stem, axis2 = RBST.stem, axis3 = K326S.stem, axis4=HD.stem)) +
    scale_x_discrete(limits = c("K326R", "RBST", "K326S","HD"), expand = c(.1, .05)) +
    geom_alluvium(aes(fill = K326R.stem))+scale_fill_manual(values = c("#F1C0C0","#B8DCE3","#AFADD2","#E5E5AF","#70C19E","#A1498F")) +
    geom_stratum(width=0.37) + geom_text(stat = "stratum", aes(label = after_stat(stratum))) +
    theme_minimal() +
    ggtitle("TEST")
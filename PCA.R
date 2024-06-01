library(tidyverse)
library(ggplot2)
library(plotly)



# Items for loop
items <- data.frame(
  Sdev_threshold = c(0, 4, 3.5, 3, 2.5, 2.2),
  File_name = c("All", "SDEV_4.0", "SDEV_3.5", "SDEV_3.0", "SDEV_2.5", "SDEV_2.2")
)

# Symbols and colors for ggplot (2D)
gg.trt.sym <- c(16, 17, 3)
gg.time.col <- c("#BC3C29", "#6F99AD", "#E18727", "#20854E", 
                 "#FFDC91", "#0072B5", "#7876B1", "#EE4C97")

# Symbols and colors for plotly (3D)
plt.trt.sym <- c("YS" = "square", "IT" = "cross", "IC" = "circle")
plt.time.col <- c("00" = "#BC3C29", "02" = "#6F99AD", "04" = "#E18727", "06" = "#20854E", 
                 "12" = "#FFDC91", "24" = "#0072B5", "48" = "#7876B1", "72" = "#EE4C97")



for (i in 1:nrow(items)){
  sdev <- items$Sdev_threshold[i]
  filename <- items$File_name[i]
  
  dir.create(filename)
  setwd(filename)
  
  
  # Prepare PCA dataset
  fpkm <- subset(log_all_genes_expression_annot, STDEV.S >= sdev) %>%
    select(1:45) %>%
    t()
  
  fpkm <- rownames_to_column(as.data.frame(fpkm), var = "ID")
  fpkm_filename <- paste0("fpkm_", filename, ".tsv")
  write_tsv(fpkm, file = fpkm_filename)
  
  fpkm.pca <- prcomp(fpkm[,-1], rank. = 10)
  
  
  # Variance explained by first several PCs
  pcvar <- fpkm.pca$sdev^2
  pcvar.pct <-  tibble(pctvar = pcvar / sum(pcvar) * 100, 
                       PC = 1:length(pcvar))
  
  scree.data <- data.frame(PC = pcvar.pct$PC[1:10],
                           pct_var = pcvar.pct$pctvar[1:10])
  scree_data_filename <- paste0("Scree_Plot_Data_", filename, ".tsv")
  write_tsv(scree.data, file = scree_data_filename)
  
  ScreePlot <- ggplot(scree.data, aes(x = PC, y = pct_var)) + 
    geom_col() + 
    theme_bw() + 
    labs(title = paste("Percent Variance Explained -", filename),
         x = "PC",
         y = "Percent Variance") + 
    scale_x_continuous(breaks = 1:10, labels = 1:10)
  scree_plot_filename <- paste0("Scree_Plot_", filename, ".png")
  ggsave(scree_plot_filename, plot = ScreePlot)
  
  
  # PCA plot dataset
  PCs <- as_tibble(fpkm.pca$x) %>%
    mutate(ID = fpkm$ID) %>%
    select(ID, everything())
  
  PCs$Treatment <- ifelse(grepl("YS", PCs$ID), "YS",
                              ifelse(grepl("IT", PCs$ID), "IT",
                                     ifelse(grepl("IC", PCs$ID), "IC", NA))) %>%
    factor()
  
  PCs$Time <- str_extract(PCs$ID, "\\d+(?=\\.)")
  for (i in 1:nrow(PCs)){
    if (nchar(PCs$Time[i]) == 1){
      PCs$Time[i] <- paste("0", PCs$Time[i], sep = "")
    }
  }
  PCs$Time <- factor(PCs$Time)
  
  PCs_filename <- paste0("PCs_", filename, ".tsv")
  write_tsv(PCs, file = PCs_filename)
  
  
  # 2D plot - PC1 vs PC2
  pca_2d_title <- paste0("PCA for ", filename, ", PC1 vs PC2")
  pca.2d <- ggplot(PCs, aes(x = PC1, y = PC2, shape = Treatment, col = Time)) + 
    geom_point() + 
    theme_bw() + 
    labs(title = pca_2d_title,
         x = paste("PC1 - ", round(pcvar.pct$pctvar[1], digits = 2), "%"),
         y = paste("PC2 - ", round(pcvar.pct$pctvar[2], digits = 2), "%")) +
    scale_shape_manual(values = gg.trt.sym) + 
    scale_color_manual(values = gg.time.col)
  
  pca_2d_filename <- paste0("PCA_for_", filename,"_PC1_vs_PC2.png")
  ggsave(pca_2d_filename, plot = pca.2d)
  
  
  # 3D plot - PC1, 2, 3
  if (sdev != 4){
    hover.info <- PCs$ID
    pca_3d_title <- paste0("PCA for ", filename, ", PC1, 2, 3")
    pca.3d <- PCs %>%
      plot_ly(
        type = "scatter3d", 
        mode = "markers", 
        x = ~PC1, 
        y = ~PC2, 
        z = ~PC3, 
        symbol = ~Treatment, 
        symbols = plt.trt.sym, 
        color = ~Time, 
        colors = plt.time.col,
        hovertext = ~hover.info
      ) %>%
      layout(title = pca_3d_title)
    
    pca_3d_filename <- paste0("PCA_for_", filename,"_PC123.html")
    htmlwidgets::saveWidget(pca.3d, file = pca_3d_filename)
  }
  
  
  setwd("..")
}

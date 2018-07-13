library(cluster)
library(factoextra)
library(magrittr)

############################## Cluster #########################################

tab <- readRDS("tab_chm_num_non_na.rds")
rownames(tab) <- make.names(tab[,1], unique = T)
tab[,c(1:4,18)] <-  NULL

x <- scale(tab)

clust <- kmeans(x = as.data.frame(x), centers = 7, iter.max = 100, nstart = 50)
clust$iter
plot(clust)

# Distance
res.dist <- get_dist(x, stand = TRUE, method = "pearson")
fviz_dist(res.dist, 
          gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# Compute hierarchical clustering
res.hc <- tab %>%
  scale() %>%                    # Scale the data
  dist(method = "euclidean") %>% # Compute dissimilarity matrix
  hclust(method = "ward.D2")     # Compute hierachical clustering
# Visualize using factoextra
# Cut in 4 groups and color by groups
fviz_dend(res.hc, k = 7, # Cut in four groups
          cex = 0.5, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07", "#000000", "#a65628", "#4daf4a"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
)

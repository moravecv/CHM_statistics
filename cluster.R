library(cluster)
library(factoextra)
library(magrittr)

############################## Cluster #########################################

################################### Non NA #####################################
### MEAN
tab <- tab_non_na ## based on pca_non_na.R
tab <- as.matrix(tab[,c(5:15)])
rownames(tab) <- make.names(names = unlist(tab_non_na[,1]), unique = T)
x <- scale(tab)
### Amelia
tab <- res_amelia ## based on pca_non_na.R
tab <- as.matrix(tab[,c(2:12)])
rownames(tab) <- make.names(names = unlist(res_amelia[,1]), unique = T)
x <- scale(tab)
### Iter
tab <- res_comp ## based on pca_non_na.R
tab <- data.frame(origin = tab.origin, tab)
tab2 <- as.matrix(tab[,c(2:12)])
rownames(tab2) <- make.names(names = unlist(tab[,1]), unique = T)
x <- scale(tab2)
################################################################################

clust <- kmeans(x = as.data.frame(x), centers = 7, iter.max = 100, nstart = 50)
clust$iter
clust$cluster

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

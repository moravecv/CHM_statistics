library(plotrix)
library(data.table)
library(fpc)
library(sparcl)
library(cluster)
library(ggbiplot)
library(ggrepel)
library(ggplot2)

tab <- readRDS("tab_chm_num.rds")

############################### PCA #############################################

# based on https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/

index <- tab[,as.numeric(which(tab[ , apply(tab, 2, function(x) !any(is.na(x)))] == TRUE))] # omit NAs
tab_df <- as.data.frame(tab) # as.data.frame
tab_df <- tab_df[,index] # select columns without NAs
index2 <- which(unlist(lapply(tab_df, class)) != "logical") # index of logical columns
tab_df <- tab_df[,index2] # omit logical columns
tab_df[tab_df == 0] <- 0.000000001 # replace 0 

log.tab <- log(tab_df[,c(5:13)]) # select columns having numbers
tab.origin <- tab_df[, 1] # column for labeling
tab.pca <- prcomp(log.tab, center = TRUE, scale. = TRUE) # PCA

print(tab.pca) 
plot(tab.pca, type = "l")
summary(tab.pca)

## plot of PCA
gg_pca_9char <- ggbiplot(tab.pca, obs.scale = 1, var.scale = 1, 
              groups = tab.origin, ellipse = TRUE, 
              circle = TRUE)
gg_pca_9char <- gg_pca_9char + scale_color_discrete(name = '')
gg_pca_9char <- gg_pca_9char + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(ggg_pca_9char)

## plot of PCA with labels
ggg_pca_9char_labs <- gg_pca_9char
ggg_pca_9char_labs <- ggg_pca_9char_labs + theme(legend.position = 'none')
ggg_pca_9char_labs <- ggg_pca_9char_labs + geom_label_repel(aes(label = tab.origin, colour = tab.origin),
                 box.padding   = 0.35, 
                 point.padding = 0.5)

print(ggg_pca_9char_labs)

##### PCA 7 characteristics          
tab_df2 <- tab_df[,c(1:11)]

log.tab2 <- log(tab_df2[,c(5:11)])
tab.origin2 <- tab_df2[, 1]
tab.pca2 <- prcomp(log.tab2, center = TRUE, scale. = TRUE) 

print(tab.pca2)
plot(tab.pca2, type = "l")
summary(tab.pca2)

ggg_pca_7char <- ggbiplot(tab.pca2, obs.scale = 1, var.scale = 1, 
              groups = tab.origin2, ellipse = TRUE, 
              circle = TRUE)
gg_pca_7char <- gg_pca_7char + scale_color_discrete(name = '')
gg_pca_7char <- gg_pca_7char + theme(legend.direction = 'horizontal', 
               legend.position = 'top')
print(g_2)

gg_pca_7char_labs <- gg_pca_7char
gg_pca_7char_labs <- gg_pca_7char_labs + theme(legend.position = 'none')
gg_pca_7char_labs <- gg_pca_7char_labs + geom_label_repel(aes(label = tab.origin2, colour = tab.origin2),
                            box.padding   = 0.35, 
                            point.padding = 0.5)

print(g2_2)

# plot each variables coefficients inside a unit circle (9 chars)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p_9chars <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(tab.pca$rotation, 
                       .names = row.names(tab.pca$rotation))
p_9chars + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")

# plot each variables coefficients inside a unit circle (7 chars)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p_7chars <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(tab.pca2$rotation, 
                       .names = row.names(tab.pca2$rotation))
p_7chars + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")

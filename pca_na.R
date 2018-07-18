library(ggbiplot)
library(ggrepel)
library(ggplot2)
library(caret)
library(factoextra)

tab <- data.frame(readRDS("tab_chm_num.rds"))

############################### PCA #############################################

# based on: https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/

tab_df <- tab[, as.vector(which(!apply(is.na(tab), 2, any) == TRUE))] # NA omit
index2 <- which(unlist(lapply(tab_df, class)) != "logical") # index of logical columns
tab_df <- tab_df[,index2] # omit logical columns
tab_df$wing <- as.numeric(tab_df$wing)

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab$new.origin <- NA
tab$new.origin[which(tab$origin == "Sumatra")] <- "Sumatra"
tab$new.origin[which(tab$origin != "Sumatra")] <- "Islands"
tab$new.origin <- as.factor(tab$new.origin)
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

tab_df <- tab_df[,c(5:10)]
tab_df2 <- tab_df + 0.000000001 # replace 0 to use log

log.tab <- log(tab_df2)
########################## wattle.front.1 ######################################
#log.tab <- log(tab_df[,c(5:11,13)]) # select columns having numbers 
######################## wattle.front.1.2. #####################################
#log.tab <- log(tab_df[,c(5:10,12:13)]) # select columns having numbers 
################################################################################

tab.origin <- tab[, 1] # column for labeling

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab.origin <- tab[, 23] # column for labeling
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

tab.pca <- prcomp(log.tab, center = TRUE, scale = TRUE) # PCA

print(tab.pca) 
plot(tab.pca, type = "l")
summary(tab.pca)

#### PCA 6 characteristics  
#### ggplot of PCA legend top
gg_pca_6char <- ggbiplot(tab.pca, obs.scale = 1, var.scale = 1, 
              groups = tab.origin, ellipse = TRUE, 
              circle = TRUE)
gg_pca_6char <- gg_pca_6char + scale_color_discrete(name = '')
gg_pca_6char <- gg_pca_6char + theme(legend.direction = 'horizontal', 
               legend.position = 'top')

print(gg_pca_6char)

#### ggplot of PCA with labels
gg_pca_6char_labs <- gg_pca_6char
gg_pca_6char_labs <- gg_pca_6char_labs + theme(legend.position = 'none')
gg_pca_6char_labs <- gg_pca_6char_labs + geom_label_repel(aes(label = tab.origin, colour = tab.origin),
                 box.padding   = 0.35, 
                 point.padding = 0.5,
                 label.size = 0.1)

print(gg_pca_6char_labs)

# plot each variables coefficients inside a unit circle (6 chars)

theta <- seq(0,2*pi,length.out = 100)
circle <- data.frame(x = cos(theta), y = sin(theta))
p_6chars <- ggplot(circle,aes(x,y)) + geom_path()

loadings <- data.frame(tab.pca$rotation, 
                       .names = row.names(tab.pca$rotation))
p_6chars + geom_text(data=loadings, 
              mapping=aes(x = PC1, y = PC2, label = .names, colour = .names)) +
  coord_fixed(ratio=1) +
  labs(x = "PC1", y = "PC2")

####### Box-Cox transformation (BlackBox thing) #######

########################## wattle.front.1 ######################################
#trans <- preProcess(tab_df[,c(5:11,13)], 
#                   method=c("BoxCox", "center", 
#                            "scale", "pca"))

########################## wattle.front.1.2 ####################################
#trans <- preProcess(tab_df[,c(5:10,12:13)], 
#                    method=c("BoxCox", "center", 
#                             "scale", "pca"))

########################## wattle.front.1 ######################################
#PC <- predict(trans, tab_df[,c(5:11,13)]) # select columns having numbers
########################## wattle.front.1.2 ####################################
#PC <- predict(trans, tab_df[,c(5:10,12:13)]) # select columns having numbers 
################################################################################

#PC2 <- data.frame(tab.origin, PC)

#centroids <- aggregate(cbind(PC1,PC2) ~ tab.origin, PC2, mean)

### ggplot with confidence ellipse 0.75
#ggplot() +
#  geom_point(data = PC2, aes(x = PC1, y = PC2, col = tab.origin)) +
#  theme(legend.position = 'none') +
#  geom_label_repel(data = PC2, aes(x = PC1, y = PC2, label = tab.origin, colour = tab.origin),
#                                                            box.padding   = 0.35, 
#                                                            point.padding = 0.5) +
#  stat_ellipse(data = PC2, aes(x = PC1, y = PC2, col = tab.origin), level = 0.75) +
#  geom_point(data = centroids, aes(x = PC1, y = PC2, col = tab.origin), shape = 3, size = 5, stroke = 2)

############################## Visualize PCA with factorextra ##################
# based on:http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

log.tab.mtx <- as.matrix(log(tab_df2))
########################## wattle.front.1 ######################################
#log.tab.mtx <- as.matrix(log(tab_df[,c(5:11,13)])) # select columns having numbers
########################## wattle.front.1.2 ######################################
#log.tab.mtx <- as.matrix(log(tab_df[,c(5:10,12:13)])) # select columns having numbers
################################################################################

row.names(log.tab.mtx) <- make.names(as.character(tab.origin), unique = TRUE)
tab.pca <- prcomp(log.tab.mtx, center = TRUE, scale = TRUE) # PCA

fviz_eig(tab.pca)

fviz_pca_ind(tab.pca,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_var(tab.pca,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

fviz_pca_biplot(tab.pca, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

eig.val <- get_eigenvalue(tab.pca)
eig.val

# Results for Variables
res.var <- get_pca_var(tab.pca)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
res.var$cor
# Results for individuals
res.ind <- get_pca_ind(tab.pca)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

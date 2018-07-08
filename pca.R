library(plotrix)
library(data.table)
library(fpc)
library(sparcl)
library(cluster)
library(ggbiplot)
library(ggrepel)
library(ggplot2)
library(caret)
library(factoextra)
library(sinkr)
library(Amelia)
library(eply)
library(missMDA)

tab <- readRDS("tab_chm_num.rds")

############################### PCA #############################################

# based on: https://tgmstat.wordpress.com/2013/11/28/computing-and-visualizing-pca-in-r/
  

index <- tab[,as.numeric(which(tab[ , apply(tab, 2, function(x) !any(is.na(x)))] == TRUE))] # omit NAs
tab_df <- as.data.frame(tab) # as.data.frame
tab_df <- tab_df[,index] # select columns without NAs
index2 <- which(unlist(lapply(tab_df, class)) != "logical") # index of logical columns
tab_df <- tab_df[,index2] # omit logical columns
tab_df[tab_df == 0] <- 0.000000001 # replace 0 

log.tab <- log(tab_df[,c(5:13)]) # select columns having numbers
tab.origin <- tab_df[, 1] # column for labeling
tab.pca <- prcomp(log.tab, center = TRUE, scale = TRUE) # PCA

print(tab.pca) 
plot(tab.pca, type = "l")
summary(tab.pca)

#### PCA 9 characteristics  
#### ggplot of PCA legend top
gg_pca_9char <- ggbiplot(tab.pca, obs.scale = 1, var.scale = 1, 
              groups = tab.origin, ellipse = TRUE, 
              circle = TRUE)
gg_pca_9char <- gg_pca_9char + scale_color_discrete(name = '')
gg_pca_9char <- gg_pca_9char + theme(legend.direction = 'horizontal', 
               legend.position = 'top')

print(gg_pca_9char)

#### ggplot of PCA with labels
gg_pca_9char_labs <- gg_pca_9char
gg_pca_9char_labs <- gg_pca_9char_labs + theme(legend.position = 'none')
gg_pca_9char_labs <- gg_pca_9char_labs + geom_label_repel(aes(label = tab.origin, colour = tab.origin),
                 box.padding   = 0.35, 
                 point.padding = 0.5)

print(gg_pca_9char_labs)

#### PCA 7 characteristics          
tab_df2 <- tab_df[,c(1:11)]

log.tab2 <- log(tab_df2[,c(5:11)])
tab.origin2 <- tab_df2[, 1]
tab.pca2 <- prcomp(log.tab2, center = TRUE, scale = TRUE) 

print(tab.pca2)
plot(tab.pca2, type = "l")
summary(tab.pca2)

#### ggplot of PCA legend top
gg_pca_7char <- ggbiplot(tab.pca2, obs.scale = 1, var.scale = 1, 
              groups = tab.origin2, ellipse = TRUE, 
              circle = TRUE)
gg_pca_7char <- gg_pca_7char + scale_color_discrete(name = '')
gg_pca_7char <- gg_pca_7char + theme(legend.direction = 'horizontal', 
               legend.position = 'top')

print(gg_pca_7char)

#### ggplot of PCA with labels
gg_pca_7char_labs <- gg_pca_7char
gg_pca_7char_labs <- gg_pca_7char_labs + theme(legend.position = 'none')
gg_pca_7char_labs <- gg_pca_7char_labs + geom_label_repel(aes(label = tab.origin2, colour = tab.origin2),
                            box.padding   = 0.35, 
                            point.padding = 0.5)

print(gg_pca_7char_labs)

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

####### Box-Cox transformation #######

trans <- preProcess(tab_df[,c(5:13)], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
PC <- predict(trans, tab_df[,c(5:13)])

PC2 <- data.frame(tab.origin, PC)

centroids <- aggregate(cbind(PC1,PC2)~tab.origin,PC2,mean)

### ggplot with confidence ellipse 0.75
ggplot() +
  geom_point(data = PC2, aes(x = PC1, y = PC2, col = tab.origin)) +
  theme(legend.position = 'none') +
  geom_label_repel(data = PC2, aes(x = PC1, y = PC2, label = tab.origin, colour = tab.origin),
                                                            box.padding   = 0.35, 
                                                            point.padding = 0.5) +
  stat_ellipse(data = PC2, aes(x = PC1, y = PC2, col = tab.origin), level = 0.75) +
  geom_point(data = centroids, aes(x = PC1, y = PC2, col = tab.origin), shape = 3, size = 5, stroke = 2)

############################## Visualize PCA with factorextra ##################
# based on:http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

log.tab.mtx <- as.matrix(log.tab)
row.names(log.tab.mtx) <- make.names(as.character(tab.origin), unique = TRUE)
tab.pca <- prcomp(log.tab.mtx, center = TRUE, scale = TRUE) # PCA

log.tab.mtx2 <- as.matrix(log.tab2)
row.names(log.tab.mtx2) <- make.names(as.character(tab.origin2), unique = TRUE)
tab.pca2 <- prcomp(log.tab.mtx2, center = TRUE, scale = TRUE) # PCA

fviz_eig(tab.pca)
fviz_eig(tab.pca2)

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

eig.val <- get_eigenvalue(tab.pca2)
eig.val

# Results for Variables
res.var <- get_pca_var(tab.pca2)
res.var$coord          # Coordinates
res.var$contrib        # Contributions to the PCs
res.var$cos2           # Quality of representation 
res.var$cor
# Results for individuals
res.ind <- get_pca_ind(tab.pca2)
res.ind$coord          # Coordinates
res.ind$contrib        # Contributions to the PCs
res.ind$cos2           # Quality of representation 

######################### Handling missing values ##############################
# based on: https://link.springer.com/article/10.1007/s11258-014-0406-z

######################### Replacing NAs by MEAN ################################

tab_non_na <- readRDS("tab_chm_num.rds")
apply(tab_non_na, 2, function(x) sum(is.na(x)))

#### tail
imput <- round(mean(tab_non_na[origin == "Simeulue", tail], na.rm = T))
tab_non_na[origin == "Simeulue" & is.na(tail), tail:= imput]
imput <- round(mean(tab_non_na[origin == "Sumatra", tail], na.rm = T))
tab_non_na[origin == "Sumatra" & is.na(tail), tail:= imput]
imput <- round(mean(tab_non_na[origin == "Mentawai", tail], na.rm = T))
tab_non_na[origin == "Mentawai" & is.na(tail), tail:= imput]
#### bill.length
imput <- mean(tab_non_na[origin == "Mentawai", bill.lenght], na.rm = T)
tab_non_na[origin == "Mentawai" & is.na(bill.lenght), bill.lenght:= imput]
#### bill.width.i.f.of.nostrils
imput <- mean(tab_non_na[origin == "Mentawai", bill.width.i.f.of.nostrils], na.rm = T)
tab_non_na[origin == "Mentawai" & is.na(bill.width.i.f.of.nostrils), bill.width.i.f.of.nostrils:= imput]
#### beak.nostril.to.peak 
imput <- mean(tab_non_na[origin == "Mentawai", beak.nostril.to.peak], na.rm = T)
tab_non_na[origin == "Mentawai" & is.na(beak.nostril.to.peak), beak.nostril.to.peak:= imput]
#### beak.height.i.f.of.nostrils
imput <- mean(tab_non_na[origin == "Mentawai", beak.height.i.f.of.nostrils], na.rm = T)
tab_non_na[origin == "Mentawai" & is.na(beak.height.i.f.of.nostrils), beak.height.i.f.of.nostrils:= imput]
#### wing.patch.perc.DORS
imput <- mean(tab_non_na[origin == "G.r.intermedia", wing.patch.perc.DORS], na.rm = T)
tab_non_na[origin == "G.r.intermedia" & is.na(wing.patch.perc.DORS), wing.patch.perc.DORS:= imput]

#### delete non needed columns
tab_non_na$wing.patch.perc.VENTR <- NULL
tab_non_na$head.hump <- NULL
tab_non_na$wattle.back.separ <- NULL

#### saveRDS 
saveRDS(object = tab_non_na, file = "tab_chm_num_non_na.rds")
tab_non_na$wattle.front.1.2. <- NULL

###### PCA
tab_non_na[tab_non_na == 0] <- 0.000000001 # replace 0 

log.tab <- log(tab_non_na[,c(5:18)]) # select columns having numbers
tab.origin <- unlist(tab_non_na[, 1]) # column for labeling
tab.pca <- prcomp(log.tab, center = TRUE, scale = TRUE) # PCA

print(tab.pca) 
plot(tab.pca, type = "l")
summary(tab.pca)

#### PCA 10 characteristics  
#### ggplot of PCA legend top
gg_pca_14char <- ggbiplot(tab.pca, obs.scale = 1, var.scale = 1, 
                         groups = tab.origin, ellipse = TRUE, 
                         circle = TRUE)
gg_pca_14char <- gg_pca_14char + scale_color_discrete(name = '')
gg_pca_14char <- gg_pca_14char + theme(legend.direction = 'horizontal', 
                                     legend.position = 'top')

print(gg_pca_14char)

#### ggplot of PCA with labels
gg_pca_14char_labs <- gg_pca_14char
gg_pca_14char_labs <- gg_pca_14char_labs + theme(legend.position = 'none')
gg_pca_14char_labs <- gg_pca_14char_labs + geom_label_repel(aes(label = tab.origin, colour = tab.origin),
                                                          box.padding   = 0.35, 
                                                          point.padding = 0.5)

print(gg_pca_14char_labs)


######################### Joint Modeling approach ##############################
# based on: https://www.jstatsoft.org/article/view/v045i07

tab_na <- readRDS("tab_chm_num.rds")
apply(tab_na, 2, function(x) sum(is.na(x)))
tab_na <- tab_na[,c(1,5:15,17,19,22)]
amelia_out <- amelia(x = tab_na, m = 5, cs = "origin")
res_amelia <- (amelia_out$imputations$imp1 + amelia_out$imputations$imp2 + amelia_out$imputations$imp3 + amelia_out$imputations$imp4 + amelia_out$imputations$imp5) / 5
res_amelia$origin <- tab_na$origin

###### PCA
res_amelia[res_amelia == 0] <- 0.000000001 # replace 0 

log.tab <- log(res_amelia[,c(2:15)]) # select columns having numbers
tab.origin <- unlist(res_amelia[, 1]) # column for labeling
tab.pca <- prcomp(log.tab, center = TRUE, scale = TRUE) # PCA

print(tab.pca) 
plot(tab.pca, type = "l")
summary(tab.pca)

#### PCA 10 characteristics  
#### ggplot of PCA legend top
gg_pca_14char <- ggbiplot(tab.pca, obs.scale = 1, var.scale = 1, 
                          groups = tab.origin, ellipse = TRUE, 
                          circle = TRUE)
gg_pca_14char <- gg_pca_14char + scale_color_discrete(name = '')
gg_pca_14char <- gg_pca_14char + theme(legend.direction = 'horizontal', 
                                       legend.position = 'top')

print(gg_pca_14char)

#### ggplot of PCA with labels
gg_pca_14char_labs <- gg_pca_14char
gg_pca_14char_labs <- gg_pca_14char_labs + theme(legend.position = 'none')
gg_pca_14char_labs <- gg_pca_14char_labs + geom_label_repel(aes(label = tab.origin, colour = tab.origin),
                                                            box.padding   = 0.35, 
                                                            point.padding = 0.5)

print(gg_pca_14char_labs)

######################### Iterative PCA method #################################
# based on: https://www.jstatsoft.org/article/view/v070i01/v70i01.pdf

tab_na <- readRDS("tab_chm_num.rds")
apply(tab_na, 2, function(x) sum(is.na(x)))
tab_na <- tab_na[,c(5:15,17,19,22)]
nb <- estim_ncpPCA(tab_na,ncp.min = 0, ncp.max=5)
res.comp <- imputePCA(tab_na,ncp=2)
res.comp <- data.frame(res.comp$completeObs)

###### PCA
res.comp[res.comp == 0] <- 0.000000001 # replace 0 

log.tab <- log(res.comp) # select columns having numbers
tab_na <- readRDS("tab_chm_num.rds")
tab.origin <- unlist(tab_na[, 1]) # column for labeling
tab.pca <- prcomp(log.tab, center = TRUE, scale = TRUE) # PCA

print(tab.pca) 
plot(tab.pca, type = "l")
summary(tab.pca)

#### PCA 10 characteristics  
#### ggplot of PCA legend top
gg_pca_14char <- ggbiplot(tab.pca, obs.scale = 1, var.scale = 1, 
                          groups = tab.origin, ellipse = TRUE, 
                          circle = TRUE)
gg_pca_14char <- gg_pca_14char + scale_color_discrete(name = '')
gg_pca_14char <- gg_pca_14char + theme(legend.direction = 'horizontal', 
                                       legend.position = 'top')

print(gg_pca_14char)

#### ggplot of PCA with labels
gg_pca_14char_labs <- gg_pca_14char
gg_pca_14char_labs <- gg_pca_14char_labs + theme(legend.position = 'none')
gg_pca_14char_labs <- gg_pca_14char_labs + geom_label_repel(aes(label = tab.origin, colour = tab.origin),
                                                            box.padding   = 0.35, 
                                                            point.padding = 0.5)

print(gg_pca_14char_labs)

library(data.table)
library(ggbiplot)
library(ggrepel) 
library(ggplot2) 
library(caret)
library(factoextra)
library(Amelia)
library(missMDA)

############################### PCA #############################################

######################### Handling missing values ##############################
# based on: https://link.springer.com/article/10.1007/s11258-014-0406-z

######################### Replacing NAs by MEAN ################################

tab_non_na <- readRDS("tab_chm_num.rds")
apply(tab_non_na, 2, function(x) sum(is.na(x)))

#### tail
#imput <- round(mean(tab_non_na[origin == "Simeulue", tail], na.rm = T))
#tab_non_na[origin == "Simeulue" & is.na(tail), tail:= imput]
#imput <- round(mean(tab_non_na[origin == "Sumatra", tail], na.rm = T))
#tab_non_na[origin == "Sumatra" & is.na(tail), tail:= imput]
#imput <- round(mean(tab_non_na[origin == "Mentawai", tail], na.rm = T))
#tab_non_na[origin == "Mentawai" & is.na(tail), tail:= imput]
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
tab_non_na$wing.patch.perc.VENTR <- NULL # 31 NAs
tab_non_na$head.hump <- NULL # logical
tab_non_na$wattle.back.separ <- NULL # logical
tab_non_na$tail <- NULL # 6 NAs

tab_non_na$wattle.front.1 <- NULL # categorical
tab_non_na$wattle.front.1.2. <- NULL # categorical
tab_non_na$wattle.front.2 <- NULL # categorical

tab_non_na$wing <- as.numeric(tab_non_na$wing)  ######### TAB FOR OTHER SCRIPTS ####

# control of NAs
apply(tab_non_na, 2, function(x) sum(is.na(x)))

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab_non_na$new.origin <- NA
tab_non_na$new.origin[which(tab_non_na$origin == "Sumatra")] <- "Sumatra"
tab_non_na$new.origin[which(tab_non_na$origin != "Sumatra")] <- "Islands"
tab_non_na$new.origin <- as.factor(tab_non_na$new.origin) ######### TAB FOR OTHER SCRIPTS ####
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

###### PCA
tab_non_na[,c(5:15)] <- tab_non_na[,c(5:15)] + 0.000000001 # replace 0
log.tab_mean <- log(tab_non_na[,c(5:15)])
########################## wattle.front.1 ######################################
#log.tab_mean <- log(tab_non_na[,c(5:17,19)]) # select columns having numbers
########################## wattle.front.1.2 ####################################
#log.tab_mean <- log(tab_non_na[,c(5:16,18:19)]) # select columns having numbers
################################################################################

tab.origin <- unlist(tab_non_na[, 1]) # column for labeling

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab.origin <- unlist(tab_non_na[, 16]) # column for labeling
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

tab.pca_mean <- prcomp(log.tab_mean, center = TRUE, scale = TRUE) # PCA

print(tab.pca_mean) 
plot(tab.pca_mean, type = "l")
summary(tab.pca_mean)

#### PCA 11 characteristics  
#### ggplot of PCA legend top
gg_pca_11char <- ggbiplot(tab.pca_mean, obs.scale = 1, var.scale = 1, 
                          groups = tab.origin, ellipse = TRUE, 
                          circle = TRUE)
gg_pca_11char <- gg_pca_11char + scale_color_discrete(name = '')
gg_pca_11char <- gg_pca_11char + theme(legend.direction = 'horizontal', 
                                       legend.position = 'top')

print(gg_pca_11char)

#### ggplot of PCA with labels
gg_pca_11char_labs <- gg_pca_11char
gg_pca_11char_labs <- gg_pca_11char_labs + theme(legend.position = 'none')
gg_pca_11char_labs <- gg_pca_11char_labs + geom_label_repel(aes(label = tab.origin, colour = tab.origin),
                                                            box.padding   = 0.35, 
                                                            point.padding = 0.5)

print(gg_pca_11char_labs)


######################### Joint Modeling approach ##############################
# based on: https://www.jstatsoft.org/article/view/v045i07

tab_na <- readRDS("tab_chm_num.rds")
apply(tab_na, 2, function(x) sum(is.na(x)))

#### delete non needed columns
tab_na$wing.patch.perc.VENTR <- NULL # 31 NAs
tab_na$head.hump <- NULL # logical
tab_na$wattle.back.separ <- NULL # logical
tab_na$tail <- NULL # 6 NAs

tab_na$wattle.front.1 <- NULL # categorical
tab_na$wattle.front.1.2. <- NULL # categorical
tab_na$wattle.front.2 <- NULL # categorical

tab_na$wing <- as.numeric(tab_na$wing) # as numeric

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab_na$new.origin <- NA
tab_na$new.origin[which(tab_na$origin == "Sumatra")] <- "Sumatra"
tab_na$new.origin[which(tab_na$origin != "Sumatra")] <- "Islands"
tab_na$new.origin <- as.factor(tab_na$new.origin)
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

tab_na <- tab_na[,c(1,5:15)]

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab_na <- tab_na[,c(16,5:15)]
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

amelia_out <- amelia(x = tab_na, m = 5, cs = "origin")

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
amelia_out <- amelia(x = tab_na, m = 5, cs = "new.origin")
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

res_amelia <- (amelia_out$imputations$imp1 + amelia_out$imputations$imp2 + 
                 amelia_out$imputations$imp3 + amelia_out$imputations$imp4 + 
                 amelia_out$imputations$imp5) / 5

res_amelia$origin <- tab_na$origin          ######### TAB FOR OTHER SCRIPTS ####
#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
res_amelia$new.origin <- tab_na$new.origin  ######### TAB FOR OTHER SCRIPTS ####
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

###### PCA
res_amelia[,c(2:12)] <- res_amelia[,c(2:12)] + 0.000000001 # replace 0 

log.tab_amelia <- log(res_amelia[,c(2:12)])
########################## wattle.front.1 ######################################
#log.tab_amelia <- log(res_amelia[,c(2:14,16)]) # select columns having numbers
########################## wattle.front.1.2 ####################################
#log.tab_amelia <- log(res_amelia[,c(2:13,15,16)]) # select columns having numbers
################################################################################

tab.origin <- unlist(res_amelia[, 1]) # column for labeling
tab.pca_amelia <- prcomp(log.tab_amelia, center = TRUE, scale = TRUE) # PCA

print(tab.pca_amelia) 
plot(tab.pca_amelia, type = "l")
summary(tab.pca_amelia)

#### PCA 11 characteristics  
#### ggplot of PCA legend top
gg_pca_11char <- ggbiplot(tab.pca_amelia, obs.scale = 1, var.scale = 1, 
                          groups = tab.origin, ellipse = TRUE, 
                          circle = TRUE)
gg_pca_11char <- gg_pca_11char + scale_color_discrete(name = '')
gg_pca_11char <- gg_pca_11char + theme(legend.direction = 'horizontal', 
                                       legend.position = 'top')

print(gg_pca_11char)

#### ggplot of PCA with labels
gg_pca_11char_labs <- gg_pca_11char
gg_pca_11char_labs <- gg_pca_11char_labs + theme(legend.position = 'none')
gg_pca_11char_labs <- gg_pca_11char_labs + geom_label_repel(aes(label = tab.origin, colour = tab.origin),
                                                            box.padding   = 0.35, 
                                                            point.padding = 0.5)

print(gg_pca_11char_labs)

######################### Iterative PCA method #################################
# based on: https://www.jstatsoft.org/article/view/v070i01/v70i01.pdf

tab_na <- readRDS("tab_chm_num.rds")
tab.origin <- unlist(tab_na[, 1]) # column for labeling
apply(tab_na, 2, function(x) sum(is.na(x)))

#### delete non needed columns
tab_na$wing.patch.perc.VENTR <- NULL # 31 NAs
tab_na$head.hump <- NULL # logical
tab_na$wattle.back.separ <- NULL # logical
tab_na$tail <- NULL # 6 NAs

tab_na$wattle.front.1 <- NULL # categorical
tab_na$wattle.front.1.2. <- NULL # categorical
tab_na$wattle.front.2 <- NULL # categorical

tab_na$wing <- as.numeric(tab_na$wing) # as numeric

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab_na$new.origin <- NA
tab_na$new.origin[which(tab_na$origin == "Sumatra")] <- "Sumatra"
tab_na$new.origin[which(tab_na$origin != "Sumatra")] <- "Islands"
tab_na$new.origin <- as.factor(tab_na$new.origin)
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

tab_na <- tab_na[,c(5:15)]

nb <- estim_ncpPCA(tab_na, ncp.min = 0, ncp.max=5)
res_comp <- imputePCA(tab_na, ncp = nb$ncp)
res_comp <- data.frame(res_comp$completeObs)  ######### TAB FOR OTHER SCRIPTS ####

###### PCA
res_comp <- res_comp + 0.000000001 # replace 0 

log.tab_iter <- log(res_comp)
########################## wattle.front.1 ######################################
#log.tab_iter <- log(res_comp[,c(1:13,15)]) # select columns having numbers
########################## wattle.front.1.2 ####################################
#log.tab_iter <- log(res_comp[,c(1:12,14,15)]) # select columns having numbers
################################################################################

tab.pca_iter <- prcomp(log.tab_iter, center = TRUE, scale = TRUE) # PCA

print(tab.pca_iter) 
plot(tab.pca_iter, type = "l")
summary(tab.pca_iter)

#### PCA 10 characteristics  
#### ggplot of PCA legend top
gg_pca_14char <- ggbiplot(tab.pca_iter, obs.scale = 1, var.scale = 1, 
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

############################## Visualize PCA with factorextra ##################
# based on:http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/118-principal-component-analysis-in-r-prcomp-vs-princomp/

log.tab.mtx <- as.matrix(log.tab_mean) # select columns having numbers

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


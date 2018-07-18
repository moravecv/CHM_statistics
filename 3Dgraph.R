library(plot3D)
library(cluster)
library(car)
library(rgl)

################## 3D Graph based on dendrogram grouping ####################

# color scheme

color = c("#e41a1c", "#377eb8","#377eb8","#377eb8","#4daf4a","#4daf4a",
          "#4daf4a","#4daf4a","#4daf4a","#4daf4a","#984ea3","#984ea3",
          "#984ea3", "#ff7f00","#ff7f00","#ff7f00","#ff7f00","#ff7f00",
          "#ff7f00","#ff7f00","#ff7f00","#ff7f00","#ff7f00","#ff7f00",
          "#ff7f00","#ff7f00","#ff7f00","#ff7f00","#ff7f00","#ff7f00",
          "#000000","#000000","#000000","#000000","#000000","#000000",
          "#000000","#000000","#a65628","#a65628")

# dendrogram

rotation <- tab.pca_amelia$rotation
a = rotation[,1]
b = rotation[,2]
c = rotation[,3] 
d = rotation[,4]
e = rotation[,5]
f = rotation[,6]

hc = hclust(dist(cbind(a, b, c, d, e, f)), method = 'ward.D')
plot(hc, axes = F, xlab = '', ylab = '', sub = '')
rect.hclust(hc, k = 3, border = 'red')

#### non NA method MEAN
########################## wattle.front.1 ######################################
tab_scale <- data.frame(scale(tab_non_na[,c(5:17,19)]))
########################## wattle.front.1.2 ####################################
#tab_scale <- data.frame(scale(tab_non_na[,c(5:16,18:19)]))

#### non NA method AMELIA
########################## wattle.front.1 ######################################
tab_scale <- data.frame(scale(res_amelia[,c(2:14,16)]))
########################## wattle.front.1.2 ####################################
#tab_scale <- data.frame(scale(res_amelia[,c(2:13,15,16)]))

#### non NA method ITERATIVE
########################## wattle.front.1 ######################################
tab_scale <- data.frame(scale(res_comp[,c(1:13,15)])) 
########################## wattle.front.1.2 ####################################
#tab_scale <- data.frame(scale(res_comp[,c(1:12,14,15)])) 

tt <- data.frame(X = rowMeans(data.frame(tab_scale$wing.patch.perc.secondaries, tab_scale$tarsus.width, 
                          tab_scale$wing, tab_scale$tail, tab_scale$width.of.scul.behind.the.eye, 
                          tab_scale$sternum, tab_scale$wing.patch.perc.DORS)),
                 Y = rowMeans(data.frame(tab_scale$tarsus.lenght, tab_scale$bill.lenght, tab_scale$beak.nostril.to.peak)),
                 Z = rowMeans(data.frame(tab_scale$wattle.front.1, tab_scale$wattle.front.2, 
                                         tab_scale$bill.width.i.f.of.nostrils, tab_scale$beak.height.i.f.of.nostrils)))

row.names(tt) <- make.names(as.character(tab.origin), unique = TRUE)

scatter3D(x = tt$X, y = tt$Y, z = tt$Z, pch = 18, cex = 2, bty = 'b2', col = color,
          theta = 45, phi = 20, ticktype = "detailed",
          xlab = "Set char 1", ylab = "Set char 2", zlab = "Set char 3", cex.lab = 0.5)

scatter3d(x = tt$X, y = tt$Y, z = tt$Z, groups = tab.origin, surface = F, ellipsoid = F, grid = FALSE, labels = as.character(tab.origin), id.n = length(tab.origin))



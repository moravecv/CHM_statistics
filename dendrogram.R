library(ggplot2)
library(ggdendro)
library(ape)
library(dendextend)

############################ Dendrogram ########################################

tab <- readRDS("tab_chm_num.rds")
rownames(tab) <- make.names(tab[,1], unique = T)
tab[,c(1:4, 16, 18, 20, 21)] <-  NULL

# Basic dendrogram plot
dist_mat <- dist(x = tab, method = "euclidean")
clust <- hclust(d = dist_mat)
plot(clust)

# Fan
plot(as.phylo(clust), type = "fan")

# Unrooted
plot(as.phylo(clust), type = "unrooted", cex = 0.6,
     no.margin = TRUE)

# ggplot function

gg_dend <- function(method_dist, method_hclust){
  dend <- tab %>% scale %>% dist(method = method_dist) %>% 
    hclust(method = method_hclust) %>% as.dendrogram %>%
    set("branches_k_color", k = 7) %>%
    set("branches_lwd", 1.2) %>%
    set("labels_colors") %>% 
    set("labels_cex", c(.9)) 
  
  Nias <- "#e41a1c"
  Bangkaru <- "#377eb8"
  Simeulue <- "#4daf4a"
  Simuk <- "#984ea3"
  Sumatra <- "#ff7f00"
  Mentawai <- "#000000"
  G.r.intermedia <- "#a65628"
  
  ggd <- as.ggdend(dend)
  
  Ni <- which(grepl(pattern = "Nias",x = ggd$labels$label) == T)
  Bg <- which(grepl(pattern = "Bangkaru",x = ggd$labels$label) == T)
  Se <- which(grepl(pattern = "Simeulue",x = ggd$labels$label) == T)
  Sk <- which(grepl(pattern = "Simuk",x = ggd$labels$label) == T)
  Su <- which(grepl(pattern = "Sumatra",x = ggd$labels$label) == T)
  Me <- which(grepl(pattern = "Mentawai",x = ggd$labels$label) == T)
  Gr <- which(grepl(pattern = "G.r.intermedia",x = ggd$labels$label) == T)
  
  ggd$labels$col[Ni] <- Nias
  ggd$labels$col[Bg] <- Bangkaru
  ggd$labels$col[Se] <- Simeulue
  ggd$labels$col[Sk] <- Simuk
  ggd$labels$col[Su] <- Sumatra
  ggd$labels$col[Me] <- Mentawai
  ggd$labels$col[Gr] <- G.r.intermedia
  
  ggplot(ggd, horiz = T) +
    theme_grey()
}

gg_dend(method_dist = "euclidean", method_hclust = "ward.D")

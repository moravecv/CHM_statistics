library(Hotelling)
library(rrcov)
library(mvnormtest)
library(stats)
library(ggplot2)
library(devtools)
library(reshape2)
library(data.table)
#devtools::install_github("kassambara/ggcorrplot")
library(ggcorrplot)

################################### Non NA #####################################
### MEAN
tab <- tab_non_na ## based on pca_non_na.R
mat <- as.matrix(tab[,c(5:15)])
origin <- tab_non_na$origin
### Amelia
tab <- res_amelia ## based on pca_non_na.R
mat <- as.matrix(tab[,c(2:12)])
origin <- res_amelia$origin
### Iter
tab <- res_comp ## based on pca_non_na.R
tab <- data.frame(origin = tab.origin, tab) ## based on pca_non_na.R
mat <- as.matrix(tab[,c(2:12)])
origin <- tab$origin

############################### CORPLOT ########################################

corr <- cor(mat)

ggcorrplot(corr, hc.order = TRUE, 
           type = "lower", 
           lab = TRUE, 
           lab_size = 3, 
           method = "circle", 
           colors = c("tomato2", "white", "springgreen3"), 
           title = "Correlogram of bird's characteristics", 
           ggtheme = theme_bw)

################################ MANOVA ########################################

man <- manova(log(mat + 0.000000001) ~ origin)
anova(man)
sum_man <- summary(man)
sum_man.aov <- summary.aov(man)
mshapiro.test(t(resid(man)))

dta_man <- data.frame(names(sum_man.aov))
dta_man$p_val <- NA

for (i in 1:nrow(dta_man)){
  p_val <- unlist(sum_man.aov[[i]][[5]][1])
  dta_man[i,2] <- p_val
}

##### Benjamini-Hochberg Procedure to decreases the false discovery rate
# based on: http://www.statisticshowto.com/benjamini-hochberg-procedure/

dta_man2 <- dta_man[order(dta_man$p_val),]
dta_man2$rank <- 1:nrow(dta_man2)
dta_man2$p.adj <- dta_man2$rank / nrow(dta_man2) * 0.05
dta_man2$pass <- dta_man2$p_val < dta_man2$p.adj

m <- {}
t <- {}

for(i in 1:11){
  m <- aov(mat[,i] ~ origin)
  t[i] <- TukeyHSD(m)
}

for (i in 1:11){
  t[[i]][,3] <- t[[i]][,4] < dta_man2$p.adj[i] 
}

o <- {}

for (i in 1:11){
  k <- data.frame(t[[i]])
  k$name <- colnames(mat)[i]
  k$relationship <- rownames(k)
  o <- rbind(o, k)
}

locations <- strsplit(o$relationship, "-")
locations <- data.frame(do.call(rbind, locations))

dta_fin <- data.frame(o, locations)
rownames(dta_fin) <- c(1:nrow(dta_fin))
colnames(dta_fin) <- c("diff", "lwr", "Difference", "p.adj", "name", "relationship", "Loc1", "Loc2")
dta_fin$diff_round <- round(dta_fin$diff, digits = 1)

dta_fin <- data.table(dta_fin)
dta_fin2 <- dta_fin[, sum(Difference), by = relationship]

loc <- strsplit(dta_fin2$relationship, "-")
loc <- data.frame(do.call(rbind, loc))
dta_fin2 <- data.frame(dta_fin2, loc)
colnames(dta_fin2) <- c("relationship", "Difference", "Loc1", "Loc2")

ggplot(dta_fin2) +
  geom_tile(aes(x = Loc1, y = Loc2, fill = Difference), colour = "white") +
  scale_fill_gradient(low = "red", high = "green") +
  geom_text(aes(x = Loc1, y = Loc2, label = Difference))

tab_bar <- data.frame(Loc = c("Bangkaru", "G.r.intermedia", "Mentawai", "Nias", 
                              "Simeulue", "Simuk", "Sumatra"), 
                      Difference = c(12, 23, 10, 4, 15, 13, 13))
ggplot(tab_bar) +
  geom_bar(aes(x = Loc, y = Difference, fill = Difference), stat = "identity") +
  scale_fill_gradient(low = "red", high = "green") 

dta_fin_plot <- dta_fin
dta_fin_plot$Difference <- as.factor(dta_fin_plot$Difference)

ggplot(dta_fin_plot) +
  geom_tile(aes(x = Loc1, y = Loc2, fill = Difference) , colour = "white") +
  scale_fill_manual(values = c("red", "green")) +
  geom_text(aes(x = Loc1, y = Loc2, label = diff_round)) +
  facet_wrap( ~ name) 

########################## T Test for two groups ###############################              

tab_non_na

tab_non_na$new.origin <- NA
tab_non_na$new.origin[which(tab_non_na$origin == "Sumatra")] <- "Sumatra"
tab_non_na$new.origin[which(tab_non_na$origin != "Sumatra")] <- "Islands"
tab_non_na$new.origin <- as.factor(tab_non_na$new.origin)

x <- tab_non_na[tab_non_na$new.origin == "Sumatra",][,5:15]
y <- tab_non_na[tab_non_na$new.origin == "Islands",][,5:15]

T2.test(x,y)

tab_non_na <- data.frame(tab_non_na)
z <- {}
for (i in 5:15){
  x <- tab_non_na[tab_non_na$new.origin == "Sumatra",][,i]
  y <- tab_non_na[tab_non_na$new.origin == "Islands",][,i]
  ttest <- t.test(x = x, y = y)
  p.val <- ttest$p.value
  k <- data.frame(name = colnames(tab_non_na)[i], p.value = p.val)
  z <- rbind(z, k)
}

##### Benjamini-Hochberg Procedure to decreases the false discovery rate
# based on: http://www.statisticshowto.com/benjamini-hochberg-procedure/
z2 <- z[order(z$p.value),]
z2$rank <- 1:nrow(z2)
z2$p.adj <- z2$rank / nrow(z2) * 0.05
z2$pass <- z2$p.value < z2$p.adj

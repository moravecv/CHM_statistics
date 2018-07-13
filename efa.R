library(psych)
library(GPArotation)
library(stats)

############################## Exploratory factor analysis #####################
# based on: https://www.promptcloud.com/blog/exploratory-factor-analysis-in-r/

fit <- factanal(x = tab, factors = 3, rotation = "varimax")
print(fit, digits = 2, cutoff = 0.3, sort = TRUE)
load <- fit$loadings[,1:2] 
plot(load, type = "n") # set up plot
text(load, labels = names(tab), cex = .7)

parallel <- fa.parallel(tab, fm = 'minres', fa = 'fa')
factors <- fa(tab, nfactors = 3, rotate = "oblimin", fm="minres")
print(factors)
print(factors$loadings,cutoff = 0.3)

factors <- fa(tab, nfactors = 7, rotate = "oblimin", fm="minres")
print(factors$loadings, cutoff = 0.3)
fa.diagram(fourfactor)

###### VSSS

cor_mat <- cor(res_amelia[,c(2:14,16)])

ll = vss(x = cor_mat, n = 4, rotate = "varimax", fm = "mle", n.obs = 40)

fa(r = cor_mat, nfactors = 5)

principal(r = cor_mat, nfactors = 5)

fa.parallel(x = cor_mat)

iclust(r.mat = cor_mat, nclusters = 7)

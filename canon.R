library(candisc)

tab <- readRDS("tab_chm_num.rds")
tab <- readRDS("tab_chm_num_non_na.rds")

############################### Canonical Function ##############################

########################## wattle.front.1 ######################################
tab_lm <- lm(cbind(tarsus.lenght, tarsus.width, sternum, wing, tail, bill.lenght, 
                    bill.width.i.f.of.nostrils, beak.nostril.to.peak, beak.height.i.f.of.nostrils, 
                    width.of.scul.behind.the.eye, wing.patch.perc.DORS, wing.patch.perc.secondaries, 
                    wattle.front.1, wattle.front.2) ~ origin, data = tab)

tab_can <- candisc(tab_lm, data = tab)

########################## wattle.front.1.2 ####################################
tab_lm <- lm(cbind(tarsus.lenght, tarsus.width, sternum, wing, tail, bill.lenght, 
                   bill.width.i.f.of.nostrils, beak.nostril.to.peak, beak.height.i.f.of.nostrils, 
                   width.of.scul.behind.the.eye, wing.patch.perc.DORS, wing.patch.perc.secondaries, 
                   wattle.front.1.2., wattle.front.2) ~ origin, data = tab)

tab_can <- candisc(tab_lm, data = tab)

################################################################################

source("gg_candisc_plot.R")

gg.candisc.plot(tab_can)

###################### TAB Eigenvalues page 62 #################################

cor <- tab_can$canrsq
eig <- tab_can$eigenvalues[1:length(cor)]
var <- eig/sum(eig) * 100
cum <- cumsum(var)

tab_res <- data.frame(Function = c(1:length(cor)), Eigenvalue = eig, Perc_of_variance = var, 
                  Cumulative_perc = cum, Canonical_correlation = cor)

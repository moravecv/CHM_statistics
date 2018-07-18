library(candisc)

# tab NA 
tab <- data.frame(readRDS("tab_chm_num.rds"))
tab_df <- tab[, as.vector(which(!apply(is.na(tab), 2, any) == TRUE))] # NA omit
index2 <- which(unlist(lapply(tab_df, class)) != "logical") # index of logical columns
tab_df <- tab_df[,index2] # omit logical columns
tab_df$wing <- as.numeric(tab_df$wing)

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab_df$new.origin <- NA
tab_df$new.origin[which(tab_df$origin=="Sumatra")] <- "Sumatra"
tab_df$new.origin[which(tab_df$origin!="Sumatra")] <- "Islands"
tab_df$new.origin <- as.factor(tab_df$new.origin)
##### !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

# tab non NA (MEAN method fill) based on pca_non_na.R
tab <- tab_non_na

# tab non NA (Joint modeling method fill) based on pca_non_na.R
tab <- res_amelia

# tab non NA (Iterative method fill) based on pca_non_na.R
tab <- res_comp

############################### Canonical Function #############################

################################ NA ############################################
tab_lm <- lm(cbind(tarsus.lenght, tarsus.width, sternum, wing, 
                   width.of.scul.behind.the.eye, wing.patch.perc.secondaries)
             ~ origin, data = tab_df)

tab_can <- candisc(mod = tab_lm, data = tab_df)

source("gg_candisc_plot.R")
gg.candisc.plot(tab_can)

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab_lm <- lm(cbind(tarsus.lenght, tarsus.width, sternum, wing, 
                   width.of.scul.behind.the.eye, wing.patch.perc.secondaries)
             ~ new.origin, data = tab_df)

tab_can <- candisc(mod = tab_lm, data = tab_df)

plot(tab_can)

############################### Non NA #########################################
tab_lm <- lm(cbind(tarsus.lenght, tarsus.width, sternum, wing, bill.lenght, 
                   bill.width.i.f.of.nostrils, beak.nostril.to.peak, beak.height.i.f.of.nostrils, 
                   width.of.scul.behind.the.eye, wing.patch.perc.DORS, wing.patch.perc.secondaries)
             ~ origin, data = tab)

tab_can <- candisc(mod = tab_lm, data = tab)

source("gg_candisc_plot.R")
gg.candisc.plot(tab_can)

#### !!!!!!!!!!!!!!!!!!!!! If two groups wanted - else skip !!!!!!!!!!!!!!!!!!!!
tab_lm <- lm(cbind(tarsus.lenght, tarsus.width, sternum, wing, bill.lenght, 
                   bill.width.i.f.of.nostrils, beak.nostril.to.peak, beak.height.i.f.of.nostrils, 
                   width.of.scul.behind.the.eye, wing.patch.perc.DORS, wing.patch.perc.secondaries)
             ~ new.origin, data = tab)

tab_can <- candisc(mod = tab_lm, data = tab)

plot(tab_can)

########################## wattle.front.1 ######################################
#tab_lm <- lm(cbind(tarsus.lenght, tarsus.width, sternum, wing, tail, bill.lenght, 
#                    bill.width.i.f.of.nostrils, beak.nostril.to.peak, beak.height.i.f.of.nostrils, 
#                    width.of.scul.behind.the.eye, wing.patch.perc.DORS, wing.patch.perc.secondaries,
#                    wattle.front.1, wattle.front.2) ~ origin, data = tab)

#tab_can <- candisc(mod = tab_lm, data = tab)

#source("gg_candisc_plot.R")
#gg.candisc.plot(tab_can)

########################## wattle.front.1.2 ####################################
#tab_lm <- lm(cbind(tarsus.lenght, tarsus.width, sternum, wing, tail, bill.lenght, 
#                   bill.width.i.f.of.nostrils, beak.nostril.to.peak, beak.height.i.f.of.nostrils, 
#                   width.of.scul.behind.the.eye, wing.patch.perc.DORS, wing.patch.perc.secondaries, 
#                   wattle.front.1.2., wattle.front.2) ~ origin, data = tab)

#tab_can <- candisc(tab_lm, data = tab)

#source("gg_candisc_plot.R")
#gg.candisc.plot(tab_can)

###################### TAB Eigenvalues page 62 #################################

cor <- tab_can$canrsq
eig <- tab_can$eigenvalues[1:length(cor)]
var <- eig/sum(eig) * 100
cum <- cumsum(var)

tab_res <- data.frame(Function = c(1:length(cor)), Eigenvalue = eig, Perc_of_variance = var, 
                  Cumulative_perc = cum, Canonical_correlation = cor)

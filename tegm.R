library(stats)

tab <- readRDS("tab_chm_num.rds")

######################## Tests of Equality of Group Means ######################

st_col <- c()
par_col1 <- c()
par_col2 <- c()
p_v_col <- c()
name_col <- c()

##### Oneway.tes excluding Nias == only one observation

# tab_chm_num.rds
cols <- colnames(tab)[c(5:14,17,19,22)]

for (i in cols){ 
  test <- oneway.test(get(i) ~ origin, data = tab[2:40,], var.equal = T, na.action = na.omit)
  st <- test$statistic
  st_col <- append(st_col, st)
  par <- test$parameter
  par_col1 <- append(par_col1, par[1])
  par_col2 <- append(par_col2, par[2])
  p_v <- test$p.value
  p_v_col <- append(p_v_col,p_v)
  name_col <- append(name_col, i)
  print(i)
}

res <- data.frame(Name = name_col, F = st_col, Num_df = par_col1, Denom_df = par_col2, P.value = p_v_col)

###### Wing patch % DORS - excluding G.r.intermedia == 2 observations, 1 NA

test <- oneway.test(wing.patch.perc.DORS ~ origin, data = tab[2:38,], var.equal = T, na.action = na.omit)

st2 <- test$statistic
par1 <- test$parameter[1]
par2 <- test$parameter[2]
p_v2 <- test$p.value

res2 <- data.frame(Name = "wing.patch.perc.DORS", F = st2, Num_df = par1, Denom_df = par2, P.value = p_v2)

res_fin <- rbind(res, res2)

# CHM_statistics

### Data
Stored in **data.zip**  
**tab_chm.rds** - raw data with NAs, comma as decimal  
**tab_chm_num.rds** - raw data with NAs, dot as decimal  
**tab_chm_num_non_na.rds** - filled data with means in groups  

### Scripts
**pca_na.R** - Principal component analysis omit NAs  
**pca_non_na.R** - Principal component analysis fill NAs - 3 methods  
**canon.R** - Canonical discriminant analysis  
**cluster.R** - k-means cluster and hierarchical clustering  
**dendrogram.R** - dendrogram visualization  
**efa.R** - Exploratory factor analysis  
**gg_candisc_plot.R** - function for ploting Canonical discriminant analysis in ggplot  
**tegm.R** - Tests of Equality of Group Means  
**3Dgraph.R** - 3D Graph based on dendrogram grouping  
**basic_stat.R** - Basic statistical figures  
**tests.R** - MANOVA, T test, Corplot  

### Short manual
Always begin with **pca_na.R** or **pca_non_na.R** to create data frames:  
**tab_non_na** - MEAN method fill  
**res_amelia** - Joint modeling method fill  
**res_comp** - Iterative method fill  
then continue with other scripts
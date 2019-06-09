### Francis Banville - Université de Montréal
### May 17th 2019


# Libraries
library(dplyr)
library(rLakeAnalyzer)

# Required R code (make_datasets.R)
source("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/R/make_datasets.R")


# Import the processed (.p) data sets 
# The data sets are in the folder "processed"
nla.2007.2012.profile.all.p = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_2012_profile_all.tsv", header = TRUE,  sep = '\t')
nla.2007.2012.profile.repeated.p = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_2012_profile_repeated.tsv", header = TRUE,  sep = '\t')

nla.2007.2012.infos.all.p = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/sites_infos/nla2007_2012_infos_all.tsv", header = TRUE,  sep = '\t')
nla.2007.2012.infos.repeated.p = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/sites_infos/nla2007_2012_infos_repeated.tsv", header = TRUE,  sep = '\t')



#### 1. Exploratory analysis ####
### Some exploratory analysis are first done on the processed data frames







### Francis Banville - Université de Montréal
### May 17th 2019

### Some exploratory analysis are first done on the processed data frames

# Libraries
library(dplyr)
library(rLakeAnalyzer)

# Import the cleaned data sets 
# The data sets are in the folder "processed"
nla.2007.profile.cleaned = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_profile_processed.tsv", header = TRUE,  sep = '\t')
nla.2012.profile.cleaned = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2012_profile_processed.tsv", header = TRUE,  sep = '\t')
nla.2007.2012.profile = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_2012_merged.tsv", header = TRUE,  sep = '\t')


# Number of sites sampled in 2007 and/or 2012
number.lakes = length(unique(nla.2007.2012.profile$SITE_ID))
cat('The number of sampled sites in 2007 and/or 2012 is:', number.lakes)

# Which sites were sampled in 2007 AND 2012
sites.sampled = nla.2007.2012.profile%>% group_by(SITE_ID, YEAR) %>% summarise()
sites.doubled = sites.sampled %>% group_by(SITE_ID) %>% summarise(n = n())
which(sites.doubled$n == 2) ## 0 ! Sites codes are not the same between 2007 and 2012
unique.2007 = unique(nla.2007.profile.cleaned$SITE_ID)
unique.2012 = unique(nla.2007.profile.cleaned$SITE_ID)





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

# How many lakes were samped in 2007 and 2012?
# Nb of resampled profiles
length(unique(nla.2007.2012.profile.repeated.p$SITE_ID))
# Nb of resampled infos
length(unique(nla.2007.2012.infos.repeated.p$SITE_ID))


# How many lakes are at least 5m deep?
# Nb of resampled lakes that are at least 5m deep
# We looked for maximum dept in 2007 since we didn't have those data in 2012
# Some lines are strangely replicated(!)
deep.sites = nla.2007.2012.infos.repeated.p %>% filter(YEAR == 2007, VISIT_NO == 1, DEPTHMAX_M >=5) %>%
  distinct()
(a = length(unique(deep.sites$SITE_ID)))
(b = length(unique(nla.2007.2012.infos.repeated.p$SITE_ID)))
a / b
# What if we look for maximum sampled depth?
deep.sample = nla.2007.2012.profile.repeated.p %>%
  group_by(SITE_ID) %>%
  summarise(max.depth = max(DEPTH)) %>%
  filter(max.depth >= 5)
(a = length(unique(deep.sample$SITE_ID)))
(b = length(unique(nla.2007.2012.profile.repeated.p$SITE_ID)))
a / b
# ANd now, what is we look for maximum sampled depth for every site (not only resampled ones)?
deep.sample.all = nla.2007.2012.profile.all.p %>%
  group_by(SITE_ID) %>%
  summarise(max.depth = max(DEPTH)) %>%
  filter(max.depth >= 5)
(a = length(unique(deep.sample.all$SITE_ID)))
(b = length(unique(nla.2007.2012.profile.all.p$SITE_ID)))
a / b



# How many lakes have a metalimnion (according to NLA layers)?
# Repeated lakes
repeated.meta = nla.2007.2012.profile.repeated.p %>%
  filter(LAYER == "M")
(a = length(unique(repeated.meta$SITE_ID)))
(b = length(unique(nla.2007.2012.profile.repeated.p$SITE_ID)))
a / b
# All lakes
all.meta = nla.2007.2012.profile.all.p %>%
  filter(LAYER == "M")
(a = length(unique(all.meta$SITE_ID)))
(b = length(unique(nla.2007.2012.profile.all.p$SITE_ID)))
a / b


# How many lakes have an hypolimnion (according to NLA layers)?
# Repeated lakes
repeated.hypo = nla.2007.2012.profile.repeated.p %>%
  filter(LAYER == "H")
(a = length(unique(repeated.hypo$SITE_ID)))
(b = length(unique(nla.2007.2012.profile.repeated.p$SITE_ID)))
a / b
# All lakes
all.hypo = nla.2007.2012.profile.all.p %>%
  filter(LAYER == "H")
(a = length(unique(all.hypo$SITE_ID)))
(b = length(unique(nla.2007.2012.profile.all.p$SITE_ID)))
a / b


# How many sites were not statified (according to NLA layers)?
# Those sites did not have any metalimnion
# Repeated lakes
(a = length(unique(repeated.meta$SITE_ID)))
(b = length(unique(nla.2007.2012.profile.repeated.p$SITE_ID)))
(c = b - a)
c / b
# All lakes
(a = length(unique(all.meta$SITE_ID)))
(b = length(unique(nla.2007.2012.profile.all.p$SITE_ID)))
(c = b - a)
c / b


# How many lakes had their epilimnion sampled (according to NLA layers)?
# The epilimnion has to be associated with a metalimnion 
# Repeated lakes
repeated.epi = nla.2007.2012.profile.repeated.p %>%
  filter(LAYER == "E")
repeated.epi.sites = unique(repeated.epi$SITE_ID)
repeated.meta.sites = unique(repeated.meta$SITE_ID)
count.epi = 0
for (i in 1:length(repeated.epi.sites)) {
  if(repeated.epi.sites[i] %in% repeated.meta.sites) {
    count.epi = count.epi + 1
  }
}
count.epi
(b = length(unique(nla.2007.2012.profile.repeated.p$SITE_ID)))
count.epi / b
# All lakes
all.epi = nla.2007.2012.profile.all.p %>%
  filter(LAYER == "E")
all.epi.sites = unique(all.epi$SITE_ID)
all.meta.sites = unique(all.meta$SITE_ID)
count.epi = 0
for (i in 1:length(all.epi.sites)) {
  if(all.epi.sites[i] %in% all.meta.sites) {
    count.epi = count.epi + 1
  }
}
count.epi
(b = length(unique(nla.2007.2012.profile.all.p$SITE_ID)))
count.epi / b


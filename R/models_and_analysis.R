### Francis Banville - Université de Montréal
### May 17th 2019


# Libraries
library(dplyr)
library(rLakeAnalyzer)

# Required R code (make_datasets.R)
# source("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/R/make_datasets.R")


# Import the processed (.p) data sets 
# The data sets are in the folder "processed"
nla.2007.2012.profile.all.p = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_2012_profile_all.tsv", header = TRUE,  sep = '\t')
nla.2007.2012.profile.repeated.p = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_2012_profile_repeated.tsv", header = TRUE,  sep = '\t')

nla.2007.2012.infos.all.p = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/sites_infos/nla2007_2012_infos_all.tsv", header = TRUE,  sep = '\t')
nla.2007.2012.infos.repeated.p = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/sites_infos/nla2007_2012_infos_repeated.tsv", header = TRUE,  sep = '\t')


View(nla.2007.2012.profile.all.p)
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


#### 2.  meta.depths() ####


## Get the depths of the top and bottom of metalimnions using meta.depths()
## NaN is returned when difference of temperatures are below the threshold value of mixed.cutoff (1 0C by default)
## The bottom depth is returned when no distinct metalimnion top and bottom were found
## Only one metalimnion was found for each combination of site, year and visit no

top.bottom.meta = nla.2007.2012.profile.all.p %>% 
  filter(!is.na(TEMP_FIELD)) %>%
  group_by(SITE_ID, VISIT_NO, YEAR) %>%
  summarize(top_depth = meta.depths(TEMP_FIELD, DEPTH)[1],
            bottom_depth = meta.depths(TEMP_FIELD, DEPTH)[2],
            max_depth = max(DEPTH))


# Change top and bottom depths to NaN when no distinct metalimnion was found
for (i in 1:nrow(top.bottom.meta)) {
  if (!is.na(top.bottom.meta$top_depth[i]) &
      !is.na(top.bottom.meta$bottom_depth[i])) {
  
  if (top.bottom.meta$top_depth[i] == 0 &
    top.bottom.meta$bottom_depth[i] == top.bottom.meta$max_depth[i])
  {
    top.bottom.meta$top_depth[i] = NaN
    top.bottom.meta$bottom_depth[i] = NaN
  } else if (top.bottom.meta$top_depth[i] == top.bottom.meta$max_depth[i] &
            top.bottom.meta$bottom_depth[i] == top.bottom.meta$max_depth[i])
           {
             top.bottom.meta$top_depth[i] = NaN
             top.bottom.meta$bottom_depth[i] = NaN
  } else if (top.bottom.meta$top_depth[i] == 0 &
      top.bottom.meta$bottom_depth[i] == 0)
  {
    top.bottom.meta$top_depth[i] = NaN
    top.bottom.meta$bottom_depth[i] = NaN
  }
}
}


# Create a layer2 variable: each observation will be assigned to the layer found using rLakeAnalyser
# e = epilimnion or no stratification
# m = metalimnion
# h = hypolimnion

nla.2007.2012.profile.all.p.2 = nla.2007.2012.profile.all.p %>%
  mutate(LAYER2 = 0)

# Warning: The for loop make take some time
# When the top and bottom of the metalimnion are at the same depth, we considered that the metalimnion 
# thickness was very small and we thus only identified the epilimnion and hypolimnion of those lakes
for (i in 1:nrow(nla.2007.2012.profile.all.p.2)) {
  site.id = nla.2007.2012.profile.all.p.2$SITE_ID[i]
  visit.no = nla.2007.2012.profile.all.p.2$VISIT_NO[i]
  year = nla.2007.2012.profile.all.p.2$YEAR[i]
  depth = nla.2007.2012.profile.all.p.2$DEPTH[i]
  
  k = which(top.bottom.meta$SITE_ID == site.id &
              top.bottom.meta$VISIT_NO == visit.no &
              top.bottom.meta$YEAR == year)
  
  top.depth = top.bottom.meta$top_depth[k]
  bottom.depth = top.bottom.meta$bottom_depth[k]
  
  if (length(k) == 0) {
    nla.2007.2012.profile.all.p.2$LAYER2[i] = "E"
  } else if (is.na(top.depth) | is.na(bottom.depth)) {
    nla.2007.2012.profile.all.p.2$LAYER2[i] = "E"
  } else if (depth < top.depth) {
    nla.2007.2012.profile.all.p.2$LAYER2[i] = "E"
  } else if (depth >= top.depth & depth <= bottom.depth) {
    nla.2007.2012.profile.all.p.2$LAYER2[i] = "M"
  } else if (depth > bottom.depth) {
    nla.2007.2012.profile.all.p.2$LAYER2[i] = "H"
  }
}

# top.bottom.meta %>% filter(top_depth == bottom_depth)
# View(nla.2007.2012.profile.all.p.2)
# View(top.bottom.meta)

# How many layers rLakeAnalyser identified differently from the NLA?
# Number of sampling event (site x year x visit no) where at least one depth have been differently layered
different = nla.2007.2012.profile.all.p.2 %>% filter(LAYER != LAYER2) %>%
  group_by(SITE_ID, VISIT_NO, YEAR) %>%
  count() %>%
  nrow()

# Total number of sampling event
total = nla.2007.2012.profile.all.p.2 %>% group_by(SITE_ID, VISIT_NO, YEAR) %>%
  count() %>%
  nrow()

# Proportion of sampling event where at least one depth have been differently layered
different / total # 59,7 %




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


#### 2.  meta.depths() (all) ####


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
different / total # 66,24 %






#### 2.  meta.depths() (repeated) ####


## Get the depths of the top and bottom of metalimnions using meta.depths()
## NaN is returned when difference of temperatures are below the threshold value of mixed.cutoff (1 0C by default)
## The bottom depth is returned when no distinct metalimnion top and bottom were found
## Only one metalimnion was found for each combination of site, year and visit no

top.bottom.meta.repeated = nla.2007.2012.profile.repeated.p %>% 
  filter(!is.na(TEMP_FIELD)) %>%
  group_by(SITE_ID, VISIT_NO, YEAR) %>%
  summarize(top_depth = meta.depths(TEMP_FIELD, DEPTH)[1],
            bottom_depth = meta.depths(TEMP_FIELD, DEPTH)[2],
            max_depth = max(DEPTH))

# Change top and bottom depths to NaN when no distinct metalimnion was found
for (i in 1:nrow(top.bottom.meta.repeated)) {
  
  if (!is.na(top.bottom.meta.repeated$top_depth[i]) &
      !is.na(top.bottom.meta.repeated$bottom_depth[i])) {
    
    if (top.bottom.meta.repeated$top_depth[i] == 0 &
        top.bottom.meta.repeated$bottom_depth[i] == top.bottom.meta.repeated$max_depth[i])
    {
      top.bottom.meta.repeated$top_depth[i] = NaN
      top.bottom.meta.repeated$bottom_depth[i] = NaN
    } else if (top.bottom.meta.repeated$top_depth[i] == top.bottom.meta.repeated$max_depth[i] &
               top.bottom.meta.repeated$bottom_depth[i] == top.bottom.meta.repeated$max_depth[i])
    {
      top.bottom.meta.repeated$top_depth[i] = NaN
      top.bottom.meta.repeated$bottom_depth[i] = NaN
    } else if (top.bottom.meta.repeated$top_depth[i] == 0 &
               top.bottom.meta.repeated$bottom_depth[i] == 0)
    {
      top.bottom.meta.repeated$top_depth[i] = NaN
      top.bottom.meta.repeated$bottom_depth[i] = NaN
    }
  }
  
}


# Create a layer2 variable: each observation will be assigned to the layer found using rLakeAnalyser
# e = epilimnion or no stratification
# m = metalimnion
# h = hypolimnion

nla.2007.2012.profile.repeated.p.2 = nla.2007.2012.profile.repeated.p %>%
  mutate(LAYER2 = 0)

# Warning: The for loop may take some time
# When the top and bottom of the metalimnion are at the same depth, we considered that the metalimnion 
# thickness was very small and we thus only identified the epilimnion and hypolimnion of those lakes
for (i in 1:nrow(nla.2007.2012.profile.repeated.p.2)) {
  site.id = nla.2007.2012.profile.repeated.p.2$SITE_ID[i]
  visit.no = nla.2007.2012.profile.repeated.p.2$VISIT_NO[i]
  year = nla.2007.2012.profile.repeated.p.2$YEAR[i]
  depth = nla.2007.2012.profile.repeated.p.2$DEPTH[i]
  
  k = which(top.bottom.meta.repeated$SITE_ID == site.id &
              top.bottom.meta.repeated$VISIT_NO == visit.no &
              top.bottom.meta.repeated$YEAR == year)
  
  top.depth = top.bottom.meta.repeated$top_depth[k]
  bottom.depth = top.bottom.meta.repeated$bottom_depth[k]
  
  if (length(k) == 0) {
    nla.2007.2012.profile.repeated.p.2$LAYER2[i] = "E"
  } else if (is.na(top.depth) | is.na(bottom.depth)) {
    nla.2007.2012.profile.repeated.p.2$LAYER2[i] = "E"
  } else if (depth < top.depth) {
    nla.2007.2012.profile.repeated.p.2$LAYER2[i] = "E"
  } else if (depth >= top.depth & depth <= bottom.depth) {
    nla.2007.2012.profile.repeated.p.2$LAYER2[i] = "M"
  } else if (depth > bottom.depth) {
    nla.2007.2012.profile.repeated.p.2$LAYER2[i] = "H"
  }
}

top.bottom.meta.repeated %>% filter(top_depth == bottom_depth)



# How many layers rLakeAnalyser identified differently from the NLA?
# Number of sampling event (site x year x visit no) where at least one depth have been differently layered
different = nla.2007.2012.profile.repeated.p.2 %>% filter(LAYER != LAYER2) %>%
  group_by(SITE_ID, VISIT_NO, YEAR) %>%
  count() %>%
  nrow()

# Total number of sampling event
total = nla.2007.2012.profile.repeated.p.2 %>% group_by(SITE_ID, VISIT_NO, YEAR) %>%
  count() %>%
  nrow()

# Proportion of sampling event where at least one depth have been differently layered
different / total # 59,95 %





##### 3. Lake types according to their layers #####

# Every sampling event will be classified related to their layers
# The types are : 
# 1 - epi - meta - hypo
# 2 - epi - meta
# 3 - meta - hypo
# 4 - epi - hypo (mini meta)
# 5 - epi 
# 6 - meta
# 7 - hypo 



# Unique sampling event 
nla.2007.2012.infos.all.u = nla.2007.2012.infos.all.p %>%
  unite(col = "SAMPLING_EVENT", SITE_ID, YEAR, VISIT_NO, sep = "_", remove = FALSE)

nla.2007.2012.infos.repeated.u = nla.2007.2012.infos.repeated.p %>%
  unite(col = "SAMPLING_EVENT", SITE_ID, YEAR, VISIT_NO, sep = "_", remove = FALSE)

top.bottom.meta.all.u = top.bottom.meta %>%
  unite(col = "SAMPLING_EVENT", SITE_ID, YEAR, VISIT_NO, sep = "_")

top.bottom.meta.repeated.u = top.bottom.meta.repeated %>%
  unite(col = "SAMPLING_EVENT", SITE_ID, YEAR, VISIT_NO, sep = "_")

sampling.event.all = unique(nla.2007.2012.infos.all.u$SAMPLING_EVENT)
sampling.event.repeated = unique(nla.2007.2012.infos.repeated.u$SAMPLING_EVENT)


top.bottom.meta.all.u = top.bottom.meta.all.u %>% 
  mutate(type = NA)

top.bottom.meta.repeated.u = top.bottom.meta.repeated.u %>%
  mutate(type = NA)

# Assign a type to all sampling event according to their layers

for (i in 1:nrow(top.bottom.meta.all.u)) {
  
  top.meta = top.bottom.meta.all.u$top_depth[i]
  bottom.meta = top.bottom.meta.all.u$bottom_depth[i]
  max.depth = top.bottom.meta.all.u$max_depth[i]
  
  if (is.na(top.meta) & is.na(bottom.meta)) {
    top.bottom.meta.all.u$type[i] = 5 # the lake is not stratified (below minimum slope, epi only)
  }
  else if (top.meta == max.depth && bottom.meta == max.depth) {
    top.bottom.meta.all.u$type[i] = 5 # the lake is not stratified (no meta found, epi only)
  }
  else if (top.meta == 0 & bottom.meta == 0) {
    top.bottom.meta.all.u$type[i] = 7 # the meta is super small at the top of the lake (hypo only)
  }
  else if (top.meta == 0 & bottom.meta == max.depth) {
    top.bottom.meta.all.u$type[i] = 6 # meta only 
  }
  else if (top.meta != 0 & top.meta == bottom.meta & bottom.meta != max.depth) {
    top.bottom.meta.all.u$type[i] = 4 # the meta is super small (epi and hypo)
  }
  else if (top.meta == 0 & bottom.meta != max.depth) {
    top.bottom.meta.all.u$type[i] = 3 # meta and hypo 
  }
  else if (top.meta != 0 & bottom.meta == max.depth) {
    top.bottom.meta.all.u$type[i] = 2 # epi and meta
  }
  else if (top.meta != 0 & bottom.meta != max.depth & top.meta != bottom.meta) {
    top.bottom.meta.all.u$type[i] = 1 # classic stratification (epi - meta - hypo)
  }
}




# Assign a type to repeated sampling event according to their layers

for (i in 1:nrow(top.bottom.meta.repeated.u)) {
  
  top.meta = top.bottom.meta.repeated.u$top_depth[i]
  bottom.meta = top.bottom.meta.repeated.u$bottom_depth[i]
  max.depth = top.bottom.meta.repeated.u$max_depth[i]
  
  if (is.na(top.meta) & is.na(bottom.meta)) {
    top.bottom.meta.repeated.u$type[i] = 5 # the lake is not stratified (below minimum slope, epi only)
  }
  else if (top.meta == max.depth && bottom.meta == max.depth) {
    top.bottom.meta.repeated.u$type[i] = 5 # the lake is not stratified (no meta found, epi only)
  }
  else if (top.meta == 0 & bottom.meta == 0) {
    top.bottom.meta.repeated.u$type[i] = 7 # the meta is super small at the top of the lake (hypo only)
  }
  else if (top.meta == 0 & bottom.meta == max.depth) {
    top.bottom.meta.repeated.u$type[i] = 6 # meta only 
  }
  else if (top.meta != 0 & top.meta == bottom.meta & bottom.meta != max.depth) {
    top.bottom.meta.repeated.u$type[i] = 4 # the meta is super small (epi and hypo)
  }
  else if (top.meta == 0 & bottom.meta != max.depth) {
    top.bottom.meta.repeated.u$type[i] = 3 # meta and hypo 
  }
  else if (top.meta != 0 & bottom.meta == max.depth) {
    top.bottom.meta.repeated.u$type[i] = 2 # epi and meta
  }
  else if (top.meta != 0 & bottom.meta != max.depth & top.meta != bottom.meta) {
    top.bottom.meta.repeated.u$type[i] = 1 # classic stratification (epi - meta - hypo)
  }
}


# Lake types as factor levels
top.bottom.meta.all.u$type = as.factor(top.bottom.meta.all.u$type)
top.bottom.meta.repeated.u$type = as.factor(top.bottom.meta.repeated.u$type)


# Add lake type to lake infos data set 

nla.2007.2012.infos.all.u.2 = left_join(nla.2007.2012.infos.all.u, top.bottom.meta.all.u, 
                                      by = "SAMPLING_EVENT") %>%
  select(-max_depth, -top_depth, -bottom_depth)


nla.2007.2012.infos.repeated.u.2 = left_join(nla.2007.2012.infos.repeated.u, top.bottom.meta.repeated.u, 
                                        by = "SAMPLING_EVENT") %>%
  select(-max_depth, -top_depth, -bottom_depth)


#### 4. approx.bathy  (cone method) (all) ####

# Maximum sampled depths (m)
max.sampled.depth.all = nla.2007.2012.profile.all.p %>% 
  group_by(SITE_ID, YEAR, VISIT_NO) %>%
  summarise(max.sampled = max(DEPTH))

# Maximum observed depths (m)
max.observed.depth.all = nla.2007.2012.infos.all.p %>%
  mutate(max.observed = DEPTHMAX_M) %>%
  select(SITE_ID, YEAR, VISIT_NO, max.observed) 

# Join maximum sampled and observed depths
max.sampled.observed.depth.all = left_join(max.sampled.depth.all, max.observed.depth.all, 
                                       by = c("SITE_ID", "YEAR", "VISIT_NO"))

# If no maximum depth is given for a specific sampling event, the maximum sampled depth will be taken
max.sampled.observed.depth.all = max.sampled.observed.depth.all %>%
  mutate(max.depth = max.observed) 

for (i in 1:nrow(max.sampled.observed.depth.all)) {
  
  if(is.na(max.sampled.observed.depth.all$max.depth[i])) {
    max.sampled.observed.depth.all$max.depth[i] = max.sampled.observed.depth.all$max.sampled[i]
  }
}

# Get rid of observations where max.depth = 0
max.sampled.observed.depth.all = max.sampled.observed.depth.all %>%
  filter(max.depth != 0)


# Lake areas (has to be in m2)
# 1 km2 = 1000^2 m2
lake.area.all = nla.2007.2012.infos.all.p %>% 
  mutate(lake.area = AREA_KM2 * 1000^2) %>%
  select(SITE_ID, YEAR, VISIT_NO, lake.area) 

# Join lake areas and maximum depths
max.depth.lake.area.all = left_join(max.sampled.observed.depth.all, lake.area.all,
                                by = c("SITE_ID", "YEAR", "VISIT_NO"))

# Estimate hypsography curves (list format)
hypsography.curves.all = list()

for (i in 1:nrow(max.depth.lake.area.all)) {

hypsography.curves.all[[i]] = approx.bathy(Zmax = max.depth.lake.area.all$max.depth[i],
                                       lkeArea = max.depth.lake.area.all$lake.area[i],
                                       method = "cone")

hypsography.curves.all[[i]]$SITE_ID = max.depth.lake.area.all$SITE_ID[i]
hypsography.curves.all[[i]]$YEAR = max.depth.lake.area.all$YEAR[i]
hypsography.curves.all[[i]]$VISIT_NO = max.depth.lake.area.all$VISIT_NO[i]

}

# Unlist the hypsography curves 
hypsography.curves.all = Reduce(bind_rows, hypsography.curves.all) %>%
  select(SITE_ID, YEAR, VISIT_NO, depths, Area.at.z)












#### 4. approx.bathy  (cone method) (repeated) ####

# Maximum sampled depths (m)
max.sampled.depth.repeated = nla.2007.2012.profile.repeated.p.2 %>% 
  group_by(SITE_ID, YEAR, VISIT_NO) %>%
  summarise(max.sampled = max(DEPTH))

# Maximum observed depths (m)
max.observed.depth.repeated = nla.2007.2012.infos.repeated.p %>%
  mutate(max.observed = DEPTHMAX_M) %>%
  select(SITE_ID, YEAR, VISIT_NO, max.observed) 

# Join maximum sampled and observed depths
max.sampled.observed.depth.repeated = left_join(max.sampled.depth.repeated, max.observed.depth.repeated, 
                                           by = c("SITE_ID", "YEAR", "VISIT_NO"))

# If no maximum depth is given for a specific sampling event, the maximum sampled depth will be taken
max.sampled.observed.depth.repeated = max.sampled.observed.depth.repeated %>%
  mutate(max.depth = max.observed) 

for (i in 1:nrow(max.sampled.observed.depth.repeated)) {
  
  if(is.na(max.sampled.observed.depth.repeated$max.depth[i])) {
    max.sampled.observed.depth.repeated$max.depth[i] = max.sampled.observed.depth.repeated$max.sampled[i]
  }
}

# Get rid of observations where max.depth = 0
max.sampled.observed.depth.repeated = max.sampled.observed.depth.repeated %>%
  filter(max.depth != 0)


# Lake areas (has to be in m2)
# 1 km2 = 1000^2 m2
lake.area.repeated = nla.2007.2012.infos.repeated.p %>% 
  mutate(lake.area = AREA_KM2 * 1000^2) %>%
  select(SITE_ID, YEAR, VISIT_NO, lake.area) 

# Join lake areas and maximum depths
max.depth.lake.area.repeated = left_join(max.sampled.observed.depth.repeated, lake.area.repeated,
                                    by = c("SITE_ID", "YEAR", "VISIT_NO"))

# Estimate hypsography curves (list format)
hypsography.curves.repeated = list()

for (i in 1:nrow(max.depth.lake.area.repeated)) {
  hypsography.curves.repeated[[i]] = approx.bathy(Zmax = max.depth.lake.area.repeated$max.depth[i],
                                             lkeArea = max.depth.lake.area.repeated$lake.area[i],
                                             method = "cone",
                                             zinterval = 0.2)
  
  hypsography.curves.repeated[[i]]$SITE_ID = max.depth.lake.area.repeated$SITE_ID[i]
  hypsography.curves.repeated[[i]]$YEAR = max.depth.lake.area.repeated$YEAR[i]
  hypsography.curves.repeated[[i]]$VISIT_NO = max.depth.lake.area.repeated$VISIT_NO[i]
  
}

# Unlist the hypsography curves 
hypsography.curves.repeated = Reduce(bind_rows, hypsography.curves.repeated) %>%
  select(SITE_ID, YEAR, VISIT_NO, depths, Area.at.z)








#### 5. Other metrics ####

# Unique sampling event 
nla.2007.2012.profile.all.u = nla.2007.2012.profile.all.p.2 %>%
  unite(col = "SAMPLING_EVENT", SITE_ID, YEAR, VISIT_NO, sep = "_")

nla.2007.2012.profile.repeated.u = nla.2007.2012.profile.repeated.p.2 %>%
  unite(col = "SAMPLING_EVENT", SITE_ID, YEAR, VISIT_NO, sep = "_")

hypsography.curves.all.u = hypsography.curves.all %>%
  unite(col = "SAMPLING_EVENT", SITE_ID, YEAR, VISIT_NO, sep = "_")

hypsography.curves.repeated.u = hypsography.curves.repeated %>%
  unite(col = "SAMPLING_EVENT", SITE_ID, YEAR, VISIT_NO, sep = "_")

sampling.event.all = unique(nla.2007.2012.profile.all.u$SAMPLING_EVENT)
sampling.event.repeated = unique(nla.2007.2012.profile.repeated.u$SAMPLING_EVENT)


# Data frame of lake metrics
nvar = 2 # Number of metrics
metrics.all = data.frame(matrix(nrow = length(sampling.event.all), ncol = nvar))
metrics.repeated = data.frame(matrix(nrow = length(sampling.event.repeated), ncol = nvar))

metrics.names = c("sampling_event", "epi_temp")
colnames(metrics.all) = metrics.names
colnames(metrics.repeated) = metrics.names

# Sampling events inside data frame
metrics.all$sampling_event = sampling.event.all
metrics.repeated$sampling_event = sampling.event.repeated



##### 5.1 Volumetrically averaged epilimnion temp #####


# All sampling events

for (i in 1:length(sampling.event.all)) {

  sampling.event = metrics.all$sampling_event[i]
  
  wtr.depths = nla.2007.2012.profile.all.u %>% 
    filter(SAMPLING_EVENT == sampling.event, !is.na(TEMP_FIELD)) 

  wtr = wtr.depths$TEMP_FIELD
  depths = wtr.depths$DEPTH

  bthA.bthD = hypsography.curves.all.u %>%
     filter(SAMPLING_EVENT == sampling.event)

  bthA = bthA.bthD$Area.at.z
  bthD = bthA.bthD$depths
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
      metrics.all$epi_temp[i] = epi.temperature(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD)
    }
}





# Repeated sampling events






for (i in 1:length(sampling.event.repeated)) {

  sampling.event = metrics.repeated$sampling_event[i]

  wtr.depths = nla.2007.2012.profile.repeated.u %>% 
  filter(SAMPLING_EVENT == sampling.event, !is.na(TEMP_FIELD)) 
  
  wtr = wtr.depths$TEMP_FIELD
  depths = wtr.depths$DEPTH

  bthA.bthD = hypsography.curves.repeated.u %>%
  filter(SAMPLING_EVENT == sampling.event)
  
  bthA = bthA.bthD$Area.at.z
  bthD = bthA.bthD$depths
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
  
    metrics.repeated$epi_temp[i] = epi.temperature(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD)
  }
    
}





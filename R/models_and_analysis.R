### Francis Banville - Université de Montréal
### May 17th 2019


# Libraries
library(dplyr)
library(rLakeAnalyzer)

# Required R code (make_datasets.R)
# source("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/R/make_datasets.R")


# Import the processed data sets 
# The data sets are in the folder "processed"
info.0712 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/info_0712.tsv", header = TRUE,  sep = '\t')
profile.0712 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/profile_0712.tsv", header = TRUE,  sep = '\t')





#### 1. Some counting ####

### Some exploratory analysis are first done on the processed data frames

# How many lakes were samped in 2007 and 2012?
# Nb of resampled profiles
length(unique(filter(profile.0712, resampled == 1)$site_id))

# Nb of resampled infos
length(unique(filter(info.0712, resampled == 1)$site_id))


# How many lakes are at least 5m deep?
# Nb of resampled lakes that are at least 5m deep
# We looked for maximum dept in 2007 since we didn't have those data in 2012
deep.sites = info.0712 %>% filter(resampled == 1, year == 2007, visit_no == 1, depthmax_m >=5) %>%
  distinct()
(a = length(unique(deep.sites$site_id)))
(b = length(unique(filter(info.0712, resampled == 1)$site_id)))
a / b

# What if we look for maximum sampled depth of resampled sites?
deep.sample = profile.0712 %>%
  filter(resampled == 1) %>%
  group_by(site_id) %>%
  summarise(max.depth = max(depth)) %>%
  filter(max.depth >= 5)
(a = length(unique(deep.sample$site_id)))
(b = length(unique(filter(profile.0712, resampled == 1)$site_id)))
a / b

# And now, what is we look for maximum sampled depth for every site (not only resampled ones)?
deep.sample.all = profile.0712 %>%
  group_by(site_id) %>%
  summarise(max.depth = max(depth)) %>%
  filter(max.depth >= 5)
(a = length(unique(deep.sample.all$site_id)))
(b = length(unique(profile.0712$site_id)))
a / b



# How many lakes have a metalimnion (according to NLA layers)?
# Repeated lakes
repeated.meta = profile.0712 %>%
  filter(resampled == 1, layer_nla == "M")
(a = length(unique(repeated.meta$site_id)))
(b = length(unique(filter(profile.0712, resampled == 1)$site_id)))
a / b

# All lakes
all.meta = profile.0712 %>%
  filter(layer_nla == "M")
(a = length(unique(all.meta$site_id)))
(b = length(unique(profile.0712$site_id)))
a / b


# How many lakes have an hypolimnion (according to NLA layers)?
# Repeated lakes
repeated.hypo = profile.0712 %>%
  filter(resampled == 1, layer_nla == "H")
(a = length(unique(repeated.hypo$site_id)))
(b = length(unique(filter(profile.0712, resampled == 1)$site_id)))
a / b

# All lakes
all.hypo = profile.0712 %>%
  filter(layer_nla == "H")
(a = length(unique(all.hypo$site_id)))
(b = length(unique(profile.0712$site_id)))
a / b


# How many sites were not statified (according to NLA layers)?
# Those sites did not have any metalimnion or hypolimnion
# Repeated lakes
repeated.non.strat = profile.0712 %>%
  filter(resampled == 1, layer_nla == "M" | layer_nla == "M")
(a = length(unique(repeated.non.strat$site_id)))
(b = length(unique(filter(profile.0712, resampled == 1)$site_id)))
(b - a) / b


# All lakes
all.non.strat = profile.0712 %>%
  filter(layer_nla == "M" | layer_nla == "M")
(a = length(unique(all.non.strat$site_id)))
(b = length(unique(profile.0712$site_id)))
(b - a) / b


# How many lakes had their epilimnion sampled (according to NLA layers)?
# The epilimnion has to be associated with a metalimnion 
# Repeated lakes
repeated.epi = profile.0712 %>%
  filter(resampled == 1, layer_nla == "E")
repeated.epi.sites = unique(repeated.epi$site_id)
repeated.meta.sites = unique(repeated.meta$site_id)
count.epi = 0
for (i in 1:length(repeated.epi.sites)) {
  if(repeated.epi.sites[i] %in% repeated.meta.sites) {
    count.epi = count.epi + 1
  }
}
count.epi
(b = length(unique(filter(profile.0712, resampled == 1)$site_id)))
count.epi / b

# All lakes
all.epi = profile.0712 %>%
  filter(layer_nla == "E")
all.epi.sites = unique(all.epi$site_id)
all.meta.sites = unique(all.meta$site_id)
count.epi = 0
for (i in 1:length(all.epi.sites)) {
  if(all.epi.sites[i] %in% all.meta.sites) {
    count.epi = count.epi + 1
  }
}
count.epi
(b = length(unique(profile.0712$site_id)))
count.epi / b








#### 2.  meta.depths()  ####


## Get the depths of the top and bottom of metalimnions using meta.depths()
## NaN is returned when difference of temperatures are below the threshold value of mixed.cutoff (1 0C by default)
## The bottom depth is returned when no distinct metalimnion top and bottom were found
## Only one metalimnion was found for each combination of site, year and visit no


top.bottom.meta = profile.0712 %>% 
  filter(!is.na(temp)) %>%
  group_by(sampling_event) %>%
  summarize(top_depth = meta.depths(temp, depth)[1],
            bottom_depth = meta.depths(temp, depth)[2],
            max_depth = max(depth)) # maximum SAMPLED depth 
                  


# Create a layer_r variable: each observation will be assigned to the layer found using rLakeAnalyser
# e = epilimnion or no stratification
# m = metalimnion
# h = hypolimnion

profile.0712 = profile.0712 %>% mutate(layer_r = NA)

# Warning: The for loop may take some time
# When the top and bottom of the metalimnion are at the same depth, we considered that the metalimnion 
# thickness was very small and we thus only identified the epilimnion and hypolimnion of those lakes
for (i in 1:nrow(profile.0712)) {
  sampling.event.i = profile.0712$sampling_event[i]
  depth = profile.0712$depth[i]
  
  k = which(top.bottom.meta$sampling_event == sampling.event.i)
  
  top.depth = top.bottom.meta$top_depth[k]
  bottom.depth = top.bottom.meta$bottom_depth[k]
  
  if (length(k) == 0) {
    profile.0712$layer_r[i] = "E"
  } else if (is.na(top.depth) | is.na(bottom.depth)) {
    profile.0712$layer_r[i] = "E"
  } else if (depth < top.depth) {
    profile.0712$layer_r[i] = "E"
  } else if (depth >= top.depth & depth <= bottom.depth) {
    profile.0712$layer_r[i] = "M"
  } else if (depth > bottom.depth) {
    profile.0712$layer_r[i] = "H"
  }
}



# How many layers rLakeAnalyser identified differently from the NLA?
# Number of sampling event where at least one depth have been differently layered
different = profile.0712 %>% filter(layer_nla != layer_r) %>%
  group_by(sampling_event) %>%
  count() %>%
  nrow()

# Total number of sampling event
total = profile.0712 %>% group_by(sampling_event) %>%
  count() %>%
  nrow()

# Proportion of sampling event where at least one depth have been differently layered
different / total # 66,24 %





# How many layers rLakeAnalyser identified differently from the NLA?
# Number of resampled sampling event where at least one depth have been differently layered
different = profile.0712 %>% filter(resampled == 1, layer_nla != layer_r) %>%
  group_by(sampling_event) %>%
  count() %>%
  nrow()

# Total number of sampling event
total = profile.0712 %>% filter(resampled == 1) %>%
  group_by(sampling_event) %>%
  count() %>%
  nrow()

# Proportion of resampled sampling event where at least one depth have been differently layered
different / total # 65,87 %





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



top.bottom.meta = top.bottom.meta %>% mutate(type = NA)



# Assign a type to all sampling event according to their layers

for (i in 1:nrow(top.bottom.meta)) {

  top.meta = top.bottom.meta$top_depth[i]
  bottom.meta = top.bottom.meta$bottom_depth[i]
  max.depth = top.bottom.meta$max_depth[i]
  
  if (is.na(top.meta) & is.na(bottom.meta)) {
    top.bottom.meta$type[i] = 5 # the lake is not stratified (below minimum slope, epi only)
  }
  else if (top.meta == max.depth && bottom.meta == max.depth) {
    top.bottom.meta$type[i] = 5 # the lake is not stratified (no meta found, epi only)
  }
  else if (top.meta == 0 & bottom.meta == 0) {
    top.bottom.meta.all.u$type[i] = 7 # the meta is super small at the top of the lake (hypo only)
  }
  else if (top.meta == 0 & bottom.meta == max.depth) {
    top.bottom.meta$type[i] = 6 # meta only 
  }
  else if (top.meta != 0 & top.meta == bottom.meta & bottom.meta != max.depth) {
    top.bottom.meta$type[i] = 4 # the meta is super small (epi and hypo)
  }
  else if (top.meta == 0 & bottom.meta != max.depth) {
    top.bottom.meta$type[i] = 3 # meta and hypo 
  }
  else if (top.meta != 0 & bottom.meta == max.depth) {
    top.bottom.meta$type[i] = 2 # epi and meta
  }
  else if (top.meta != 0 & bottom.meta != max.depth & top.meta != bottom.meta) {
    top.bottom.meta$type[i] = 1 # classic stratification (epi - meta - hypo)
  }
}



# Lake types as factor levels
top.bottom.meta$type = as.factor(top.bottom.meta$type)



# Add lake type to info.0712 data set 

info.0712 = left_join(info.0712, top.bottom.meta, by = "sampling_event") %>%
  select(-max_depth)




#### 4. approx.bathy  (cone method)  ####

# Maximum sampled depths (m)
max.sampled.depth = profile.0712 %>% 
  group_by(sampling_event) %>%
  summarise(max.sampled = max(depth))

# Maximum observed depths (m)
max.observed.depth = info.0712 %>%
  mutate(max.observed = depthmax_m) %>%
  select(sampling_event, max.observed) 

# Join maximum sampled and observed depths
max.sampled.observed.depth = left_join(max.sampled.depth, max.observed.depth, 
                                       by = "sampling_event")

# If no maximum depth is given for a specific sampling event, the maximum sampled depth will be taken
max.sampled.observed.depth = max.sampled.observed.depth %>%
  mutate(max.depth = max.observed) 

for (i in 1:nrow(max.sampled.observed.depth)) {
  
  if(is.na(max.sampled.observed.depth$max.depth[i])) {
    max.sampled.observed.depth$max.depth[i] = max.sampled.observed.depth$max.sampled[i]
  }
}

# Get rid of observations where max.depth = 0
max.sampled.observed.depth = max.sampled.observed.depth %>%
  filter(max.depth != 0)


# Lake areas (has to be in m2)
# 1 km2 = 1000^2 m2
lake.area = info.0712 %>% 
  mutate(lake.area = area_km2 * 1000^2) %>%
  select(sampling_event, lake.area) 

# Join lake areas and maximum depths
max.depth.lake.area = left_join(max.sampled.observed.depth, lake.area,
                                by = "sampling_event")

# Estimate hypsography curves (list format)
hypsography.curves = list()

for (i in 1:nrow(max.depth.lake.area)) {

hypsography.curves[[i]] = approx.bathy(Zmax = max.depth.lake.area$max.depth[i],
                                       lkeArea = max.depth.lake.area$lake.area[i],
                                       method = "cone")

hypsography.curves[[i]]$sampling_event = max.depth.lake.area$sampling_event[i]

}

# Unlist the hypsography curves 
hypsography.curves = Reduce(bind_rows, hypsography.curves) %>%
  select(sampling_event, depths, Area.at.z)














#### 5. Lake metrics ####


# Data frame of lake metrics
nvar = 4 # Number of metrics
lake.metrics = data.frame(matrix(nrow = length(unique(info.0712$sampling_event)), ncol = nvar))

colnames(lake.metrics) = c("sampling_event", "epitemp_C", "metatemp_C", "hypotemp_C")


# Sampling events inside data frame
lake.metrics$sampling_event = unique(info.0712$sampling_event)



##### 5.1 Volumetrically averaged layer temp #####


# Volumetrically averaged epilimnion temp 

for (i in 1:length(unique(info.0712$sampling_event))) {

  sampling.event = lake.metrics$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) 

  wtr = wtr.depths$temp
  depths = wtr.depths$depth

  bthA.bthD = hypsography.curves %>%
     filter(sampling_event == sampling.event)

  bthA = bthA.bthD$Area.at.z
  bthD = bthA.bthD$depths
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
      lake.metrics$epitemp_C[i] = epi.temperature(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD)
    }
}





# Volumetrically averaged metalimnion temp 


meta.temperature <- function(wtr, depths, bthA, bthD){ # function to computer the averaged metalimnion temp 
  
  md = rLakeAnalyzer::meta.depths(wtr, depths)
  
  if(is.na(md[1])){
    avg_temp = NA
  }else{
    avg_temp = layer.temperature(md[1],md[2], wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
  }
  return(avg_temp)
}



for (i in 1:length(unique(info.0712$sampling_event))) {
  
  sampling.event = lake.metrics$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) 
  
  wtr = wtr.depths$temp
  depths = wtr.depths$depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event)
  
  bthA = bthA.bthD$Area.at.z
  bthD = bthA.bthD$depths
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    lake.metrics$metatemp_C[i] = meta.temperature(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD)
  }
}



# Volumetrically averaged hypolimnion temp 

for (i in 1:length(unique(info.0712$sampling_event))) {
  
  sampling.event = lake.metrics$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) 
  
  wtr = wtr.depths$temp
  depths = wtr.depths$depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event)
  
  bthA = bthA.bthD$Area.at.z
  bthD = bthA.bthD$depths
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    lake.metrics$hypotemp_C[i] = hypo.temperature(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD)
  }
}


# Join those volumetrically averaged temp to the info.0712 data set
info.0712 = left_join(info.0712, lake.metrics, by = "sampling_event")

# Compute difference of temperature between top and bottom layer
# If only one volumetrically layer averaged temp is present, delaT = 0
# If 2 volumetrically layer averaged temp are present, deltaT = top layer T - bottom layer T
# If the 3 volumetrically layer averaged temp are present, deltaT = epi T - hypo T 
# If none are present, deltaT = NA

info.0712 = info.0712 %>% mutate(deltaT_C = NA)

for (i in 1:nrow(info.0712)) {
  epitemp.i = info.0712$epitemp_C[i]
  metatemp.i = info.0712$metatemp_C[i]
  hypotemp.i = info.0712$hypotemp_C[i]

  if (is.na(epitemp.i) & is.na(metatemp.i) & is.na(hypotemp.i)) {
    info.0712$deltaT_C[i] = NA
  } else if (!is.na(epitemp.i) & !is.na(hypotemp.i)) {
    info.0712$deltaT_C[i] = epitemp.i - hypotemp.i
  } else if (!is.na(epitemp.i) & !is.na(metatemp.i)) {
    info.0712$deltaT_C[i] = epitemp.i - metatemp.i 
  } else if (!is.na(metatemp.i) & !is.na(hypotemp.i)) {
    info.0712$deltaT_C[i] = metatemp.i - hypotemp.i
  } else {
    info.0712$deltaT_C[i] = 0 
  }
}






#### 5.2 Epilimnion thickness ####

info.0712 = info.0712 %>% mutate(epithick_m = NA) 

# Epilimnion thickness (m)
# If no stratification was observed (top_depth = NA), the epilimnion thickness equals the max sampled depth
# If a stratification was observed, the epilimnion thickness equals the top depth of the metalimnion
for (i in 1:nrow(info.0712)) {
  if(is.na(info.0712$top_depth[i])) {
    info.0712$epithick_m[i] = info.0712$sampled_depthmax_m[i]
  } else {
    info.0712$epithick_m[i] = info.0712$top_depth[i]
  }
}

# Epilimnion thickness : maximum sampled depth ratio 
info.0712 = info.0712 %>% mutate(epithick_pct = epithick_m / sampled_depthmax_m)






##### 5.3 Average layer density #####


layer.dens= data.frame(matrix(nrow = length(unique(info.0712$sampling_event)), ncol = 4))

colnames(layer.dens) = c("sampling_event", "epidens_kgm3", "metadens_kgm3", "hypodens_kgm3")


# Sampling events inside data frame
layer.dens$sampling_event = unique(info.0712$sampling_event)


# Epilimnion density 
epi.density <- function(wtr, depths, bthA, bthD){ # function to computer the density of the epilimnion  
    
    md = rLakeAnalyzer::meta.depths(wtr, depths)
    
    if(is.na(md[1])){
     dens = layer.density(0,max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
    }else{
      dens = layer.density(0,md[1], wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
    }
    return(dens)
  }



for (i in 1:length(unique(info.0712$sampling_event))) {
  
  sampling.event = layer.dens$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) 
  
  wtr = wtr.depths$temp
  depths = wtr.depths$depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event)
  
  bthA = bthA.bthD$Area.at.z
  bthD = bthA.bthD$depths
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    layer.dens$epidens_kgm3[i] = epi.density(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD)
  }
}








# Metalimnion density 
meta.density <- function(wtr, depths, bthA, bthD){ # function to computer the density of the metalimnion  
  
  md = rLakeAnalyzer::meta.depths(wtr, depths)
  
  if(is.na(md[1])){
    dens = NA
  }else{
    dens = layer.density(md[1],md[2], wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
  }
  return(dens)
}



for (i in 1:length(unique(info.0712$sampling_event))) {
  
  sampling.event = layer.dens$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) 
  
  wtr = wtr.depths$temp
  depths = wtr.depths$depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event)
  
  bthA = bthA.bthD$Area.at.z
  bthD = bthA.bthD$depths
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    layer.dens$metadens_kgm3[i] = meta.density(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD)
  }
}









# Hypolimnion density 
hypo.density <- function(wtr, depths, bthA, bthD){ # function to computer the density of the hypolimnion 
  
  md = rLakeAnalyzer::meta.depths(wtr, depths)
  
  if(is.na(md[2])){
    dens = NA
  }else{
    dens = layer.density(md[2],max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
  }
  return(dens)
}



for (i in 1:length(unique(info.0712$sampling_event))) {
  
  sampling.event = layer.dens$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) 
  
  wtr = wtr.depths$temp
  depths = wtr.depths$depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event)
  
  bthA = bthA.bthD$Area.at.z
  bthD = bthA.bthD$depths
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    layer.dens$hypodens_kgm3[i] = hypo.density(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD)
  }
}





# Join those averaged density to the info.0712 data set
info.0712 = left_join(info.0712, layer.dens, by = "sampling_event")

# Compute difference of density between top and bottom layer
# If only one layer density is present, deltaD = 0
# If 2 layer densities are present, deltaD = top layer density - bottom layer density
# If the 3 layer densities are present, deltaD = epi density - hypo density  
# If none are present, deltaD = NA

info.0712 = info.0712 %>% mutate(deltaD_kgm3 = NA)

for (i in 1:nrow(info.0712)) {
  epidens.i = info.0712$epidens_kgm3[i]
  metadens.i = info.0712$metadens_kgm3[i]
  hypodens.i = info.0712$hypodens_kgm3[i]
  
  if (is.na(epidens.i) & is.na(metadens.i) & is.na(hypodens.i)) {
    info.0712$deltaD_kgm3[i] = NA
  } else if (!is.na(epidens.i) & !is.na(hypodens.i)) {
    info.0712$deltaD_kgm3[i] = hypodens.i - epidens.i
  } else if (!is.na(epidens.i) & !is.na(metadens.i)) {
    info.0712$deltaD_kgm3[i] = metadens.i - epidens.i 
  } else if (!is.na(metadens.i) & !is.na(hypodens.i)) {
    info.0712$deltaD_kgm3[i] = hypodens.i - metadens.i
  } else {
    info.0712$deltaD_kgm3[i] = 0 
  }
}





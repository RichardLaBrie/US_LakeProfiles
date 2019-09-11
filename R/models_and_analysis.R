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





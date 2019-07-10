### Francis Banville - Université de Montréal
### July 10th 2019

### The interim data sets, obtained from OpenRefine, are here tidied and merged to form processed data sets
### The sections of our script first refer to the indicators identified by the NARS 
### Then they represent the merging of data sets and the disctinction between repeated and non repeated sites


# Libraries
library("dplyr")
library("tidyr")




#### 1. Water chemisty (profiles) ####



#### 1.1 Tidy the profile.07 data set 


# Import the interim data sets 
profile.07 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/water_chemistry/nla2007_profile_20091008.tsv", header = TRUE,  sep = '\t')

colnames(profile.07) = tolower(colnames(profile.07)) # variable names in lowercase
names(profile.07)[names(profile.07) == "temp_field"] = "temp"
names(profile.07)[names(profile.07) == "do_field"] = "DO"
names(profile.07)[names(profile.07) == "ph_field"] = "PH"
names(profile.07)[names(profile.07) == "cond_field"] = "cond"


profile.07 = profile.07 %>%
  # Observations without site_id or depth values are removed 
  filter(site_id != "", !is.na(depth)) %>%
  # Unuseful variables are also removed
  select(-sampled_profile, -flag_profile, -comment_profile) %>%
  # Sort by site_id, visit_no and depth for each site
  arrange(site_id, visit_no, depth)


profile.07 = profile.07 %>%
  # Average observations on same site, visit no and depth 
  group_by(site_id, visit_no, depth) %>%
  summarise(year = year[1], 
            month = month[1], 
            day = day[1], 
            metalimnion = metalimnion[1],
            temp = mean(temp),
            DO = mean(DO),
            PH = mean(PH),
            cond = mean(cond)
  )


# Create a layer variable: each observation will be assigned to a layer
# e = epilimnion 
# m = metalimnion
# h = hypolimnion

profile.07 = profile.07 %>%
  mutate(layer_nla = NA)

profile.07$layer_nla[1] = "E"

for (i in 2:nrow(profile.07)) {
  # an observation that is at the top or bottom of the metalimnion is in the metalimnion
  if (profile.07$metalimnion[i] == "T" | 
      profile.07$metalimnion[i] == "B")
  { profile.07$layer_nla[i] = "M" }
  
  # observations below the bottom of the metalimnion are in the hypolimnion
  else if (profile.07$site_id[i-1] == profile.07$site_id[i] &
           profile.07$visit_no[i-1] == profile.07$visit_no[i] &
           profile.07$metalimnion[i-1] == "B")
  { profile.07$layer_nla[i] = "H" }
  
  # observations on the same site and visit are in the same layer as the one above them
  # (if they don't cross the top or bottom of the metalimnion)
  else if(profile.07$site_id[i-1] == profile.07$site_id[i] &
          profile.07$visit_no[i-1] == profile.07$visit_no[i]) 
  { profile.07$layer_nla[i] = profile.07$layer_nla[i-1] } 
  
  # observations on another site or visit start at the epilimnion
  else  { profile.07$layer_nla[i] = "E" }
}



# Remove unuseful variable
profile.07 = profile.07 %>%
  select(-metalimnion)


# Change variables class
profile.07$site_id = as.factor(profile.07$site_id)
profile.07$visit_no = as.factor(profile.07$visit_no)
profile.07$depth = as.numeric(profile.07$depth)
profile.07$year = as.numeric(profile.07$year)
profile.07$month = as.numeric(profile.07$month)
profile.07$day = as.numeric(profile.07$day)
profile.07$temp = as.numeric(profile.07$temp)
profile.07$DO = as.numeric(profile.07$DO)
profile.07$PH = as.numeric(profile.07$PH)
profile.07$cond = as.numeric(profile.07$cond)
profile.07$layer_nla = as.factor(profile.07$layer_nla)






#### 1.2 Tidy the profile.12 data set 

# Import the interim data sets 
profile.12 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/water_chemistry/nla2012_wide_profile_08232016.tsv", header = TRUE,  sep = '\t')


colnames(profile.12) = tolower(colnames(profile.12)) # variable names in lowercase
names(profile.12)[names(profile.12) == "temp_field"] = "temp"
names(profile.12)[names(profile.12) == "do_field"] = "DO"
names(profile.12)[names(profile.12) == "ph_field"] = "PH"
names(profile.12)[names(profile.12) == "cond_field"] = "cond"
names(profile.12)[names(profile.12) == "date_profile.1"] = "year"


 
profile.12 = profile.12 %>%
  # Only PROF observations are kept (we don't want calibration values)
  # Observations without site_id or depth values are removed 
  filter(site_id != "", sample_type == "PROF", !is.na(depth)) %>%
  select(-sample_type) %>%
  # Sort by depth for each site
  arrange(site_id, visit_no, depth)


profile.12 = profile.12 %>%
  # Average observations on same site, visit no and depth 
  group_by(site_id, visit_no, depth) %>%
  summarise(year = year[1], 
            month = month[1], 
            day = day[1], 
            metalimnion = metalimnion[1],
            temp = mean(temp),
            DO = mean(DO),
            PH = mean(PH),
            cond = mean(cond)
            )


# Create a layer variable: each observation will be assigned to a layer
# e = epilimnion 
# m = metalimnion
# h = hypolimnion

profile.12 = profile.12 %>%
  mutate(layer_nla = NA)

profile.12$layer_nla[1] = "E"

for (i in 2:nrow(profile.12)) {
  # an observation that is at the top or bottom of the metalimnionit is in the metalimnion
  if (profile.12$metalimnion[i] == "T" | 
           profile.12$metalimnion[i] == "B")
  { profile.12$layer_nla[i] = "M" }
  
  # observations below the bottom of the metalimnion are in the hypolimnion
  else if (profile.12$site_id[i-1] == profile.12$site_id[i] &
           profile.12$visit_no[i-1] == profile.12$visit_no[i] &
           profile.12$metalimnion[i-1] == "B")
  { profile.12$layer_nla[i] = "H" }
  
  # observations on the same site and visit are in the same layer as the one above them
  # (if they don't cross the top or bottom of the metalimnion)
  else if(profile.12$site_id[i-1] == profile.12$site_id[i] &
     profile.12$visit_no[i-1] == profile.12$visit_no[i]) 
  { profile.12$layer_nla[i] = profile.12$layer_nla[i-1] } 
  
  # observations on another site or visit start at the epilimnion
  else  { profile.12$layer_nla[i] = "E" }
}
  


# Remove unuseful variable
profile.12 = profile.12 %>%
  select(-metalimnion)

profile.12$site_id = as.factor(profile.12$site_id)
profile.12$visit_no = as.factor(profile.12$visit_no)
profile.12$depth = as.numeric(profile.12$depth)
profile.12$year = as.numeric(profile.12$year)
profile.12$month = as.numeric(profile.12$month)
profile.12$day = as.numeric(profile.12$day)
profile.12$temp = as.numeric(profile.12$temp)
profile.12$DO = as.numeric(profile.12$DO)
profile.12$PH = as.numeric(profile.12$PH)
profile.12$cond = as.numeric(profile.12$cond)
profile.12$layer_nla = as.factor(profile.12$layer_nla)







#### 2. Site information ####


#### 2.1 Tidy the info.07 data set 


# Import the interim data set 
info.07 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/site_information/nla2007_sampledlakeinformation_20091113.tsv", header = TRUE,  sep = '\t')


colnames(info.07) = tolower(colnames(info.07)) # variable names in lowercase


# Select useful variables
info.07 = info.07 %>%
  select(site_id, visit_no, day, month, year, lat_dd, lon_dd,
         st, epa_reg, wsa_eco9, huc_2, huc_8, lake_origin, 
         lakearea, lakeperim, elev_pt, depthmax, sld,
         wgt_nla)


# Rename columns 
colnames(info.07) = c("siteid_07", "visit_no", "day", "month", "year", "lat", "lon",
                               "state", "EPA_reg", "ECO9", "HUC2", "HUC8", "lake_origin", 
                               "area_km2", "perim_km", "elevation_m", "depthmax_m", "SLD",
                               "WGT")

# Change the variables' class
info.07  = info.07 %>%
  mutate(siteid_07 = as.factor(siteid_07),
         visit_no = as.numeric(visit_no), 
         day = as.numeric(day), month = as.numeric(month), year = as.numeric(year),
         lat = as.numeric(lat), lon = as.numeric(lon), 
         state = as.factor(state),
         EPA_reg = as.factor(EPA_reg), ECO9 = as.factor(ECO9),
         HUC2 = as.factor(HUC2), HUC8 = as.factor(HUC8),
         lake_origin = as.factor(lake_origin), 
         area_km2 = as.numeric(area_km2), perim_km = as.numeric(perim_km), 
         elevation_m = as.numeric(elevation_m), 
         depthmax_m = as.numeric(depthmax_m), SLD = as.numeric(SLD), 
         WGT = as.numeric(WGT))


info.07$EPA_reg = tolower(info.07$EPA_reg) # values in lowercase letters
info.07$lake_origin = tolower(info.07$lake_origin) # values in lowercase letters






#### 2.2 Tidy the info.12 data set 

# Import the interim data set 
info.12 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/site_information/nla2012_wide_siteinfo_08232016.tsv", header = TRUE,  sep = '\t', quote = "\\")


colnames(info.12) = tolower(colnames(info.12)) # variable names in lowercase

# Select useful variables
# The 2012 data set does not have information on maximum depth nor on shoreline development (SLD)
info.12 = info.12 %>%
  select(site_id, siteid_07, sitesamp, visit_no, day, month, year, lat_dd83, lon_dd83,
         state, epa_reg, fw_eco9, huc2, huc8, lake_origin, 
         area_ha, perim_km, elevation, 
         wgt_all) 

# Rename columns according to the ones of the 2007 data set
colnames(info.12) = c("siteid_12", "siteid_07", "sitesamp", "visit_no", "day", "month", "year", "lat", "lon",
                               "state", "EPA_reg", "ECO9", "HUC2", "HUC8", "lake_origin", 
                               "area_km2", "perim_km", "elevation_m",
                               "WGT")


# Change the variables' class
# Filter for sampled sites only
# 1 km2 = 1000 ha2
info.12  = info.12 %>%
  mutate(siteid_12 = as.factor(siteid_12), siteid_07 = as.factor(siteid_07), 
         sitesamp = as.factor(sitesamp), visit_no = as.numeric(visit_no), 
         day = as.numeric(day), month = as.numeric(month), year = as.numeric(year),
         lat = as.numeric(lat), lon = as.numeric(lon), 
         state = as.factor(state),
         EPA_reg = as.factor(EPA_reg), ECO9 = as.factor(ECO9),
         HUC2 = as.factor(HUC2), HUC8 = as.factor(HUC8),
         lake_origin = as.factor(lake_origin), 
         area_km2 = as.numeric(area_km2) / 100, perim_km = as.numeric(perim_km), 
         elevation_m = as.numeric(elevation_m), 
         WGT = as.numeric(WGT)) %>% 
  filter(sitesamp == "Y") %>%
  select(-sitesamp)



info.12$EPA_reg = tolower(info.12$EPA_reg) # values in lowercase letters
info.12$lake_origin = tolower(info.12$lake_origin) # values in lowercase letters





#### 3. Secchi ####


#### 3.1 Tidy the secchi.07 data set 

# Import the interim data set 
secchi.07 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/secchi/nla2007_secchi_20091008.tsv", header = TRUE,  sep = '\t', quote = "\\")

colnames(secchi.07) = tolower(colnames(secchi.07)) # variable names in lowercase


names(secchi.07)[names(secchi.07) == "secmean_m"] = "secchi_m"
names(secchi.07)[names(secchi.07) == "site_id"] = "siteid_07"

 

secchi.07$clear_to_bottom = as.character(secchi.07$clear_to_bottom)

# Replace "Y" by 1s in binary variable clear_to_bottom
secchi.07$clear_to_bottom = replace(secchi.07$clear_to_bottom, which(secchi.07$clear_to_bottom == "Y"), 1)

# Replace empty values of variable clear_to_bottom by 0 when Secchi depth was measured
secchi.07$clear_to_bottom = replace(secchi.07$clear_to_bottom, which(!is.na(secchi.07$secchi_m)), 0)

# Change the variables' class
secchi.07 = secchi.07 %>% mutate(
  siteid_07 = as.factor(siteid_07), 
  year = as.numeric(year), visit_no = as.numeric(visit_no), 
  month = as.numeric(month), day = as.numeric(day), 
  secchi_m = as.numeric(secchi_m), clear_to_bottom = as.numeric(clear_to_bottom)
)




#### 3.2 Tidy the secchi.12 data set 

# Import the interim data set 
secchi.12 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/secchi/nla2012_secchi_08232016.tsv", header = TRUE,  sep = '\t', quote = "\\")

colnames(secchi.12) = tolower(colnames(secchi.12)) # variable names in lowercase

names(secchi.12)[names(secchi.12) == "secchi"] = "secchi_m"
names(secchi.12)[names(secchi.12) == "site_id"] = "siteid_12"


secchi.12$clear_to_bottom = as.character(secchi.12$clear_to_bottom)


# Replace "Y" by 1s in binary variable clear_to_bottom
secchi.12$clear_to_bottom = replace(secchi.12$clear_to_bottom, which(secchi.12$clear_to_bottom == "Y"), 1)

# Replace empty values of variable clear_to_bottom by 0 when Secchi depth was measured
secchi.12$clear_to_bottom = replace(secchi.12$clear_to_bottom, which(!is.na(secchi.12$secchi_m)), 0)

# Change the variables' class
secchi.12 = secchi.12 %>% mutate(
  siteid_12 = as.factor(siteid_12), 
  year = as.numeric(year), visit_no = as.numeric(visit_no), 
  month = as.numeric(month), day = as.numeric(day), 
  secchi_m = as.numeric(secchi_m), clear_to_bottom = as.numeric(clear_to_bottom)
)














#### JE REPRENDS ICI DEMAIN ###### X Merged data sets ####


#### X.1 Merged site information


# Join site information with Secchi depths

info.07u = full_join(info.07, secchi.07, by = c("siteid_07", "day", "month", "year", "visit_no")) 
info.12u = full_join(info.12, secchi.12, by = c("siteid_12", "day", "month", "year", "visit_no")) 


# Join the 2007 and 2012 sampling events
info.0712u = bind_rows(info.07u, info.12u)


# All NA years are in 2012
# Change those NAs to 2012
info.0712u$year = replace(info.0712u$year, which(is.na(info.0712u$year)), 2012)

info.0712$site_id_07 = as.character(info.0712$site_id_07)
info.0712$site_id_12 = as.character(info.0712$site_id_12)


# Assign adequate ID for sites
# If a site sampled in 2012 was also sampled in 2007, we will keep its 2007 id
info.0712u = info.0712u %>% mutate(site_id = NA)

for (i in 1:nrow(info.0712u)) {
  if (is.na(info.0712$site_id_07[i])) {
    info.0712$site_id[i] = info.0712$site_id_12[i]
  } else {
    info.0712$site_id[i] = info.0712$site_id_07[i]
  }

}
   



info.0712 = bind_rows(nla.2007.2012.infos.repeated, nla.2007.infos.3) %>%
  bind_rows(nla.2012.infos.3)


# Assign adequate ID for sites



# Correctly order observations and remove identical rows
nla.2007.2012.infos.all = nla.2007.2012.infos.all %>%
  select(-SITEID_07, -SITEID_12, -SITESAMP) %>%
  arrange(SITE_ID, YEAR, VISIT_NO) %>%
  distinct()


# Export the processed data set
write.table(nla.2007.2012.infos.all,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/sites_infos/nla2007_2012_infos_all.tsv",
            sep = "\t")


#### X Complete lake profiles data set ####

# Merge lake profiles data set
nla.2007.2012.profile = union(nla.2007.profile.5, nla.2012.profile.5)

# Assign adequate class to SITE_ID
nla.2007.2012.profile$SITE_ID = as.factor(nla.2007.2012.profile$SITE_ID)


# Change the sites id. of 2012 to those of 2007 when resampled
for (i in 1:nrow(nla.2007.2012.profile)) {
  if (nla.2007.2012.profile$SITE_ID[i] %in% common.sites$SITEID_12)
  {
    k = which(as.character(common.sites$SITEID_12) == as.character(nla.2007.2012.profile$SITE_ID[i]))
    nla.2007.2012.profile$SITE_ID[i] = as.character(common.sites$SITEID_07[k])
  }
}

# Reorder obervations
nla.2007.2012.profile.2 = nla.2007.2012.profile %>%
  arrange(SITE_ID, YEAR, VISIT_NO, DEPTH)

# Export the processed data set
write.table(nla.2007.2012.profile.2,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_2012_profile_all.tsv",
            sep = "\t")













#### X. Resampled sites ####
######  JE VEUX UNE COLONNE AVEC RESAMPLED 1/0


# Data table to link sites sampled in 2007 to those sampled in 2012 (from site_information)
common.sites = inner_join(info.07, info.12, by = "site_id_07") %>%
  select(site_id_07, site_id_12) %>%
  distinct(site_id_07, site_id_12, .keep_all = TRUE)
dim(common.sites) # 401 repeated sites


# Join the infos data set and only keep the sites repeated in 2007 and 2012
info.0712 = inner_join(info.07, info.12, by = "site_id_07")


# 2007 data table of sites repeated in 2012
nla.2007.infos.repeated = nla.2007.2012.infos %>%
  select(SITEID_07, ends_with("x"), DEPTHMAX_M, SLD)
colnames(nla.2007.infos.repeated) = c("SITEID_07", "VISIT_NO", "DAY", "MONTH", "YEAR", "LAT", "LON",
                                "STATE", "EPA_REG", "ECO9", "HUC2", "HUC8", "LAKE_ORIGIN", 
                                "AREA_KM2", "PERIM_KM", "ELEVATION_M", "WGT", "DEPTHMAX_M", "SLD")

# 2012 data table of sites also sampled in 2007
nla.2012.infos.repeated = nla.2007.2012.infos %>%
  select(SITEID_07, SITEID_12, ends_with("y"))
colnames(nla.2012.infos.repeated) = c("SITEID_07", "SITEID_12", "VISIT_NO", "DAY", "MONTH", "YEAR", "LAT", "LON",
                                "STATE", "EPA_REG", "ECO9", "HUC2", "HUC8", "LAKE_ORIGIN", 
                                "AREA_KM2", "PERIM_KM", "ELEVATION_M", "WGT")

# Join the 2 preceding data tables
nla.2007.2012.infos.repeated = bind_rows(nla.2007.infos.repeated, nla.2012.infos.repeated)


# Change the sites id. of 2012 to those of 2007
for (i in 1:nrow(nla.2007.2012.infos.repeated)) {
  if (nla.2007.2012.infos.repeated$SITEID_12[i] %in% common.sites$SITEID_12)
  {
    k = which(as.character(common.sites$SITEID_12) == as.character(nla.2007.2012.profile$SITEID_12[i]))
    nla.2007.2012.profile$SITEID_07[i] = as.character(common.sites$SITEID_07[k])
  }
}

# Keep a single column for SITE_ID
nla.2007.2012.infos.repeated = nla.2007.2012.infos.repeated %>%
  mutate(SITE_ID = SITEID_07) %>%
  select(-SITEID_07, -SITEID_12)

# Correctly order observations
nla.2007.2012.infos.repeated = nla.2007.2012.infos.repeated %>%
  arrange(SITE_ID, YEAR, VISIT_NO) %>%
  distinct()

# All NA years are in 2012
# Change those NAs to 2012
which.NA.2012 = which(is.na(nla.2007.2012.infos.repeated$YEAR)) 
nla.2007.2012.infos.repeated[which.NA.2012, "YEAR"] = 2012



# Export the processed data set
write.table(nla.2007.2012.infos.repeated,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/sites_infos/nla2007_2012_infos_repeated.tsv",
            sep = "\t")







#### X Repeated lake profiles ####

# Keeping sites whom infos was taken in 2007 and 2012
# (from SITEID_07)
nla.2007.2012.profile.repeated = nla.2007.2012.profile.2 %>% filter(SITE_ID %in% common.sites$SITEID_07 | 
                                                                      SITE_ID %in% common.sites$SITEID_12)


# Change the sites id. of 2012 to those of 2007
for (i in 1:nrow(nla.2007.2012.profile.repeated)) {
  if (nla.2007.2012.profile.repeated$SITE_ID[i] %in% common.sites$SITEID_12)
  {
    k = which(as.character(common.sites$SITEID_12) == as.character(nla.2007.2012.profile.repeated$SITE_ID[i]))
    nla.2007.2012.profile.repeated$SITE_ID[i] = as.character(common.sites$SITEID_07[k])
  }
}


# Reorder obervations
nla.2007.2012.profile.repeated = nla.2007.2012.profile.repeated %>%
  arrange(SITE_ID, YEAR, VISIT_NO, DEPTH)

# Export the processed data set
write.table(nla.2007.2012.profile.repeated,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_2012_profile_repeated.tsv",
            sep = "\t")



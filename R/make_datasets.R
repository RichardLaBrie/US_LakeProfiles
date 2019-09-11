### Francis Banville - Université de Montréal
### August 27th 2019

### The interim data sets, obtained from OpenRefine, are here tidied and merged to form processed data sets
### The sections of our script first refer to the indicators identified by the NARS 
### Then they represent the merging of data sets 

### The numbers in the data frame names represent the year of the sampling events
### For example, profile.07 is the data frame of profiles sampled in 2007
### profile.12 is the data frame of profiles sampled in 2012


# Libraries
library("dplyr")
library("stringr")
library("tidyr")



#### 1. Water chemisty (profiles) ####



#### 1.1 Tidy the profile.07 data set 


# Import the interim data sets 
profile.07 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/water_chemistry/nla2007_profile_20091008.tsv", header = TRUE,  sep = '\t')

# Rename some variables 
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
  # Average observations on same site, visit no and depth (observations)
  # An observation cannot have multiple values
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
  mutate(layer_nla = NA) # layer_nla is the layer assigned by the NLA

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
         wgt_all, uid) 

# Rename columns according to the ones of the 2007 data set
colnames(info.12) = c("siteid_12", "siteid_07", "sitesamp", "visit_no", "day", "month", "year", "lat", "lon",
                               "state", "EPA_reg", "ECO9", "HUC2", "HUC8", "lake_origin", 
                               "area_km2", "perim_km", "elevation_m",
                               "WGT", "UID")


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
         WGT = as.numeric(WGT), UID = as.factor(UID)) %>% 
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
  secchi_m = as.numeric(secchi_m), clear_to_bottom = as.numeric(clear_to_bottom)
) %>%
  select(-day, -month)




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
  secchi_m = as.numeric(secchi_m), clear_to_bottom = as.numeric(clear_to_bottom)
) %>%
  select(-day, -month)











#### 4. Landscape data ####


#### 4.1 Tidy the landscape.07 data set 

# Import the interim data set 
landscape.07 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/landscape_data/nla2007_basin_landuse_metrics_20061022.tsv", header = TRUE,  sep = '\t', quote = "\\")

colnames(landscape.07) = tolower(colnames(landscape.07)) # variable names in lowercase


names(landscape.07)[names(landscape.07) == "pct_forest_bsn"] = "pct_forest" # % forested basin area
names(landscape.07)[names(landscape.07) == "pct_agric_bsn"] = "pct_agric" # % agriculture basin area
names(landscape.07)[names(landscape.07) == "site_id"] = "siteid_07" 


# Select useful variables
landscape.07 = landscape.07 %>% 
  select(siteid_07, basinarea_km2, pct_forest, pct_agric) %>%
  mutate(year = 2007)


# Change the variables' class
landscape.07 = landscape.07 %>% mutate(
  siteid_07 = as.factor(siteid_07), 
  year = as.numeric(year),  
  basinarea_km2 = as.numeric(basinarea_km2), 
  pct_forest = as.numeric(pct_forest), 
  pct_agric = as.numeric(pct_agric)) 








#### 4.2 Tidy the landscape.12 data set 


# Import the interim data set 
landscape.12 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/landscape_data/nla2012_wide_watershed.tsv", header = TRUE,  sep = '\t', quote = "\\")

colnames(landscape.12) = tolower(colnames(landscape.12)) # variable names in lowercase

names(landscape.12)[names(landscape.12) == "nlcd2001_forestpct_bsn"] = "pct_forest" # % forested basin area
names(landscape.12)[names(landscape.12) == "nlcd2006_agricpct_bsn"] = "pct_agric" # % agriculture basin area
names(landscape.12)[names(landscape.12) == "site_id"] = "siteid_12" 



# Select useful variables
landscape.12 = landscape.12 %>% 
  select(siteid_12, 
         nlcd2001_11pct_bsn,  nlcd2001_11area_bsn,
         pct_forest, pct_agric) %>%
  mutate(year = 2012)


# Compute the watershed area 
# The basin area is missing in the 2012 data set 
# It will be computed as the ratio of the total open water area (in m2) to the % of open water area of the basin
landscape.12 = landscape.12 %>% 
  mutate(basinarea_km2 = nlcd2001_11area_bsn / nlcd2001_11pct_bsn / 10000) %>% # 1 m2 = 1/10000 km2
  select(-nlcd2001_11area_bsn, -nlcd2001_11pct_bsn)



# Change the variables' class
landscape.12 = landscape.12 %>% mutate(
  siteid_12 = as.factor(siteid_12), 
  year = as.numeric(year), 
  basinarea_km2 = as.numeric(basinarea_km2), 
  pct_forest = as.numeric(pct_forest), 
  pct_agric = as.numeric(pct_agric)) 









#### 5. Water chemistry (water quality) ####


#### 5.1 Tidy the chemistry.07 data set 

# Import the interim data set 
chemistry.07 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/water_chemistry/NLA2007_WaterQuality_20091123.tsv", header = TRUE,  sep = '\t', quote = "\\")

colnames(chemistry.07) = tolower(colnames(chemistry.07)) # variable names in lowercase


names(chemistry.07)[names(chemistry.07) == "chla"] = "chla_ugL" # Chlorophyll a concentration (µg/L).  
names(chemistry.07)[names(chemistry.07) == "color"] = "color_PCU" # Color (PCU)
names(chemistry.07)[names(chemistry.07) == "ntl"] = "NTL_ugL" # Total Nitrogen (ug/L)
names(chemistry.07)[names(chemistry.07) == "ptl"] = "PTL_ugL" # Total Phosphorus (ug/L)
names(chemistry.07)[names(chemistry.07) == "na_ppm"] = "NA_mgL" # Sodium (mg/L)
names(chemistry.07)[names(chemistry.07) == "doc"] = "DOC_mgL" #  Dissolved Organic Carbon (mg/L)
names(chemistry.07)[names(chemistry.07) == "cond"] = "cond_uScm" # Conductivity (uS/cm @ 25 C)
names(chemistry.07)[names(chemistry.07) == "turb"] = "turb_NTU" # Turbidity (NTU)	
names(chemistry.07)[names(chemistry.07) == "site_id"] = "siteid_07" 


# Select useful variables
chemistry.07 = chemistry.07 %>% 
  select(siteid_07, year, visit_no, 
         chla_ugL, color_PCU, NTL_ugL, PTL_ugL, NA_mgL, DOC_mgL,  
         cond_uScm, turb_NTU) 

# Change the variables' class
chemistry.07 = chemistry.07 %>% mutate(
  siteid_07 = as.factor(siteid_07), 
  year = as.numeric(year),  
  visit_no = as.numeric(visit_no),
  chla_ugL = as.numeric(chla_ugL), color_PCU = as.numeric(color_PCU),
  NTL_ugL = as.numeric(NTL_ugL), PTL_ugL = as.numeric(PTL_ugL), 
  NA_mgL = as.numeric(NA_mgL), DOC_mgL = as.numeric(DOC_mgL),
  cond_uScm = as.numeric(cond_uScm), turb_NTU = as.numeric(turb_NTU))




# Lake nutrient-color status assignment
# Methods of Nürnberg and Shaw (1998) and Webster et al. (2008)
chemistry.07 = chemistry.07 %>%
  mutate(nutrient_color = NA)

for (i in 1:nrow(chemistry.07)) {
  TP = chemistry.07$PTL_ugL[i]
  color = chemistry.07$color_PCU[i]
  
  if (TP <= 30 & color <= 20) {
    chemistry.07$nutrient_color[i] = "blue"
  } else if (TP > 30 & color <= 20) {
    chemistry.07$nutrient_color[i] = "green"
  } else if (TP <= 30 & color > 20) {
    chemistry.07$nutrient_color[i] = "brown"
  } else if (TP > 30 & color > 20) {
    chemistry.07$nutrient_color[i] = "murky"
  } 
}





#### 5.2 Tidy the chemistry.12 data set 

# Import the interim data set 
chemistry.12 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/water_chemistry/nla2012_waterchem_wide.tsv", header = TRUE,  sep = '\t', quote = "\\")

colnames(chemistry.12) = tolower(colnames(chemistry.12)) # variable names in lowercase


names(chemistry.12)[names(chemistry.12) == "color_result"] = "color_PCU" # Color (PCU)
names(chemistry.12)[names(chemistry.12) == "ntl_result"] = "NTL_mgL" # Total Nitrogen (ug/L)
names(chemistry.12)[names(chemistry.12) == "ptl_result"] = "PTL_ugL" # Total Phosphorus (ug/L)
names(chemistry.12)[names(chemistry.12) == "sodium_result"] = "NA_mgL" # Sodium (mg/L)
names(chemistry.12)[names(chemistry.12) == "doc_result"] = "DOC_mgL" #  Dissolved Organic Carbon (mg/L)
names(chemistry.12)[names(chemistry.12) == "cond_result"] = "cond_uScm" # Conductivity (uS/cm @ 25 C)
names(chemistry.12)[names(chemistry.12) == "turb_result"] = "turb_NTU" # Turbidity (NTU)	
names(chemistry.12)[names(chemistry.12) == "uid"] = "UID" 


# Select useful variables 
chemistry.12 = chemistry.12 %>% 
  mutate(NTL_ugL = NTL_mgL * 1000) %>% # total nitrogen was expressed in mg/L in the 2012 data set and in ug/L in the 2007 one
  select(UID, color_PCU, NTL_ugL, PTL_ugL, NA_mgL, DOC_mgL,  
         cond_uScm, turb_NTU) 

# Change the variables' class
chemistry.12 = chemistry.12 %>% mutate(
  UID = as.factor(UID), color_PCU = as.numeric(color_PCU),
  NTL_ugL = as.numeric(NTL_ugL), PTL_ugL = as.numeric(PTL_ugL), 
  NA_mgL = as.numeric(NA_mgL), DOC_mgL = as.numeric(DOC_mgL), 
  cond_uScm = as.numeric(cond_uScm), turb_NTU = as.numeric(turb_NTU))



# Lake nutrient-color status assignment
# Methods of Nürnberg and Shaw (1998) and Webster et al. (2008)
chemistry.12 = chemistry.12 %>%
  mutate(nutrient_color = NA)

for (i in 1:nrow(chemistry.12)) {
  TP = chemistry.12$PTL_ugL[i]
  color = chemistry.12$color_PCU[i]
  
  if (is.na(TP) | is.na(color)) {
    chemistry.12$nutrient_color[i] = NA
  } else if (TP <= 30 & color <= 20) {
    chemistry.12$nutrient_color[i] = "blue"
  } else if (TP > 30 & color <= 20) {
    chemistry.12$nutrient_color[i] = "green"
  } else if (TP <= 30 & color > 20) {
    chemistry.12$nutrient_color[i] = "brown"
  } else if (TP > 30 & color > 20) {
    chemistry.12$nutrient_color[i] = "murky"
  } 
}





#### 5.3 Tidy the chla.12 data set 

# Chla data were already present in the chemistry.07 data set 

# Import the interim data set 
chla.12 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/chlorophyll-a/nla2012_chla_wide.tsv", header = TRUE,  sep = '\t', quote = "\\")

colnames(chla.12) = tolower(colnames(chla.12)) # variable names in lowercase


names(chla.12)[names(chla.12) == "chlx_result"] = "chla_ugL" # Analyte value for X-site chlorophyll a (ug/L)
names(chla.12)[names(chla.12) == "uid"] = "UID" 


# Select useful variables 
chla.12 = chla.12 %>% 
  select(UID, chla_ugL) 

# Change the variables' class
chla.12 = chla.12 %>% mutate(
  UID = as.factor(UID), chla_ugL = as.numeric(chla_ugL))



# Merge chla.12 to chemistry.12
chemistry.12 = left_join(chemistry.12, chla.12, by = "UID")







#### 6. Climate ####


#### 6.1 Climate data from the NOAA 


# Import the raw data sets
# state monthly precipitation (124 years)
precip.124y = read.csv("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/raw/climate_noaa/110-pcp.csv", header = TRUE, skip = 3)
# state montly average temperature (124 years)
avgtemp.124y = read.csv("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/raw/climate_noaa/110-tavg.csv", header = TRUE, skip = 3)
# state montly minimum temperature (124 years)
mintemp.124y = read.csv("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/raw/climate_noaa/110-tmin.csv", header = TRUE, skip = 3)
# state montly maximum temperature (124 years)
maxtemp.124y = read.csv("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/raw/climate_noaa/110-tmax.csv", header = TRUE, skip = 3)



# Select useful variables 
precip.124y = precip.124y %>% select(Location, Date, Value)
avgtemp.124y = avgtemp.124y %>% select(Location, Date, Value)
mintemp.124y = mintemp.124y %>% select(Location, Date, Value)
maxtemp.124y = maxtemp.124y %>% select(Location, Date, Value)

# Change the units of the variables and rename them
precip.124y = precip.124y %>% mutate(precip_mm = 25.4 * Value) %>% # 1 inch = 25.4 mm
  select(-Value)
avgtemp.124y = avgtemp.124y %>% mutate(avgtemp_C = (Value - 32) * 5/9) %>% # fahrenheit to celsius
  select(-Value)
mintemp.124y = mintemp.124y %>% mutate(mintemp_C = (Value - 32) * 5/9) %>% # fahrenheit to celsius
  select(-Value)
maxtemp.124y = maxtemp.124y %>% mutate(maxtemp_C = (Value - 32) * 5/9) %>% # fahrenheit to celsius
  select(-Value)


# Join the climate data sets
climate.124y = precip.124y %>% left_join(avgtemp.124y, by = c("Location", "Date")) %>%
  left_join(mintemp.124y, by = c("Location", "Date")) %>%
  left_join(maxtemp.124y, by = c("Location", "Date")) 

# Separate the date column into year and month columns
climate.124y$Date = as.numeric(climate.124y$Date)
climate.124y = separate(climate.124y, col = Date, into = c("year", "month"), 
                        sep = 4, remove = TRUE)

# Retain data in 2007 and 2012 only
climate.0712 = climate.124y %>% filter(year == 2007 | year == 2012)




# States abbreviations
states = read.csv("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/raw/climate_noaa/states.csv", header = TRUE)

names(states)[names(states) == "State"] = "Location" # State name
names(states)[names(states) == "Abbreviation"] = "state" # State abbreviation

climate.0712 = left_join(climate.0712, states, by = "Location") %>%
  select(-Location)









#### 7 Merged data sets ####


#### 7.1  Merged profile data sets 


# Merge profiles data set
profile.0712 = bind_rows(profile.07, profile.12)


# The variable 'resampled' indicates whether or not the sites were sampled in both 2007 ans 2012
profile.0712 = profile.0712 %>% mutate(resampled = NA)

for (i in 1:nrow(profile.0712)) {
  if (profile.0712$site_id[i] %in% common.sites$siteid_07 | 
      profile.0712$site_id[i] %in% common.sites$siteid_12) {
    profile.0712$resampled[i] = 1
  } else {
    profile.0712$resampled[i] = 0
  }
}


# Change the site id to the site unique identifier defined above
# In other words, change the id of the sites sampled in 2012 to the id they had in 
# 2007 when they were also sampled in 2007


for (i in 1:nrow(profile.0712)) {
  if (profile.0712$resampled[i] == 1 & profile.0712$year[i] == 2012) {
    k = which(common.sites$siteid_12 == profile.0712$site_id[i])
    profile.0712$site_id[i] = common.sites$siteid_07[k]
  }
}



# Assign adequate class to site_id
profile.0712$site_id = as.factor(profile.0712$site_id)


# Assign a unique identifier for every sampling event 
profile.0712 = profile.0712 %>% unite("sampling_event", site_id, year, visit_no, sep = "-", remove = FALSE)


# Reorder obervations
profile.0712 = profile.0712 %>%
  arrange(site_id, year, visit_no, depth)




# SOme mistakes were indroduced in the data 
# For the sampling event "NLA06608-1868-2012-2", at a depth of 4.02 m, the T is 3075 oC
# It should be 30,75 oC
# For the other events, we can't assume any realistic observations

profile.0712$temp[which(profile.0712$temp > 3075)] = 30.75
profile.0712$temp[which(profile.0712$temp > 60)] = NA


# Export the processed data set
write.table(profile.0712,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/profile_0712.tsv",
            sep = "\t")






#### 7.2 Merged site information


# Join site information with Secchi depths

info.07u = left_join(info.07, secchi.07, by = c("siteid_07", "year", "visit_no")) %>%
  left_join(landscape.07, by = c("siteid_07", "year")) %>%
  left_join(chemistry.07, by = c("siteid_07", "year", "visit_no"))

info.12u = left_join(info.12, secchi.12, by = c("siteid_12", "year", "visit_no")) %>%
  left_join(landscape.12, by = c("siteid_12", "year")) %>%
  left_join(chemistry.12, by = "UID")


# Join the 2007 and 2012 sampling events
info.0712 = bind_rows(info.07u, info.12u)


# Compute the watershed area (WA) to lake area (LA) ratio
info.0712 = info.0712 %>%
  mutate(WALA_ratio = basinarea_km2 / area_km2)


# The NA year is in 2012
# Change this NA to 2012
info.0712$year = replace(info.0712$year, which(is.na(info.0712$year)), 2012)



# Assign adequate ID for sites
# If a site sampled in 2012 was also sampled in 2007, we will keep its 2007 id
# The variable siteid thus represent a site unique identifier
info.0712 = info.0712 %>% mutate(site_id = NA)

for (i in 1:nrow(info.0712)) {
  if (info.0712$siteid_07[i] == "") {
    info.0712$site_id[i] = info.0712$siteid_12[i]
  } else {
    info.0712$site_id[i] = info.0712$siteid_07[i]
  }

}



# Assign a unique identifier for every sampling event 
info.0712 = info.0712 %>% unite("sampling_event", site_id, year, visit_no, sep = "-", remove = FALSE)


# Add maximum sampled depth from profile.0712
info.0712 = left_join(info.0712, 
                      profile.0712 %>% group_by(sampling_event) %>% summarise(sampled_depthmax_m = max(depth)),
                      by = "sampling_event")


# Compute the Shoreline development index (SLD) in 2012 (=LAKEPERIM/(2*sqrt(LAKEAREA*pi))
# The SLD indices given in the 2007 site information data set are the same as those computed here 

info.0712 = info.0712 %>% 
  mutate(SLD = perim_km / (2 * sqrt(area_km2 * pi)))










# Compute the mean climatic observations over a 150 days period prior to the sampling event
info.0712 = info.0712 %>% mutate(precip_mm = NA, avgtemp_C = NA, mintemp_C = NA, maxtemp_C = NA)

climate.W = matrix(0, nrow(info.0712), 12) # montly ponderation matrix
colnames(climate.W) = c("jan", "feb", "mar", "apr", 
                          "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
rownames(climate.W) = info.0712$sampling_event
climate.W = as.data.frame(climate.W)



for (i in 1:nrow(info.0712)) {
  year = info.0712$year[i]
  month = info.0712$month[i]
  day = info.0712$day[i]

  if (!is.na(year) & !is.na(month) & !is.na(day)) {
    
  date.sampled = date(paste(year, month, day, sep = "-")) # date of sampling event
  date.150d = date.sampled - 149 # date 150 days before
  
  pond.in.month = as.data.frame(table(as.yearmon(seq(date.150d, date.sampled, "day")))) %>% # nb of days in each month between the 2 dates 
    separate(Var1, into = c("month", "year"), sep = " ") %>%
    select(-year)
  
  # Nb of days of each month inside climate.W matrix
    
  if(nrow(filter(pond.in.month, month == "janv.")) != 0)
  {climate.W$jan[i] = filter(pond.in.month, month == "janv.")$Freq}
 
  if(nrow(filter(pond.in.month, month == "févr.")) != 0)
  {climate.W$feb[i] = filter(pond.in.month, month == "févr.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "mars")) != 0)
  {climate.W$mar[i] = filter(pond.in.month, month == "mars")$Freq}
  
  if(nrow(filter(pond.in.month, month == "avr.")) != 0)
  {climate.W$apr[i] = filter(pond.in.month, month == "avr.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "mai")) != 0)
  {climate.W$may[i] = filter(pond.in.month, month == "mai")$Freq}
  
  if(nrow(filter(pond.in.month, month == "juin")) != 0)
  {climate.W$jun[i] = filter(pond.in.month, month == "juin")$Freq}
  
  if(nrow(filter(pond.in.month, month == "juil.")) != 0)
  {climate.W$jul[i] = filter(pond.in.month, month == "juil.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "août")) != 0)
  {climate.W$aug[i] = filter(pond.in.month, month == "août")$Freq}
  
  if(nrow(filter(pond.in.month, month == "sept.")) != 0)
  {climate.W$sep[i] = filter(pond.in.month, month == "sept.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "oct.")) != 0)
  {climate.W$oct[i] = filter(pond.in.month, month == "oct.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "nov.")) != 0)
  {climate.W$nov[i] = filter(pond.in.month, month == "nov.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "déc.")) != 0)
  {climate.W$dec[i] = filter(pond.in.month, month == "déc.")$Freq}
}

}

climate.W = decostand(climate.W, "total", MARGIN = 1) # proportion of the 150 days period




# Mean climate metrics for 150 days before each sampling event 
# Each row represent a sampling event 
for (i in 1:nrow(info.0712)) {
  state.i = as.character(info.0712$state[i])
  year.i = info.0712$year[i]
  
  climate.i = climate.0712 %>% filter(state == state.i, year == year.i)
  climate.W.i = t(climate.W[i,])
  
  info.0712$precip_mm[i] = sum(climate.i$precip_mm * climate.W.i)
  info.0712$avgtemp_C[i] = sum(climate.i$avgtemp_C * climate.W.i)
  info.0712$mintemp_C[i] = sum(climate.i$mintemp_C * climate.W.i)
  info.0712$maxtemp_C[i] = sum(climate.i$maxtemp_C * climate.W.i)
}









# Correction of inconsistencies between levels of lake_origin
info.0712$lake_origin = str_replace_all(info.0712$lake_origin, "_", "-")


# Identify sites sampled in both 2007 and 2012
common.sites = inner_join(info.07, info.12, by = "siteid_07") %>%
  select(siteid_07, siteid_12) %>%
  distinct(siteid_07, siteid_12, .keep_all = TRUE)
dim(common.sites) # 401 repeated sites


# Complete missing siteid_12 values when sampled in 2007
for (i in 1:nrow(info.0712)) {
  if (info.0712$siteid_07[i] %in% common.sites$siteid_07) {
    k = which(common.sites$siteid_07 == info.0712$siteid_07[i])
    info.0712$siteid_12[i] = as.character(common.sites$siteid_12[k])
  } 
}

# The variable 'resampled' indicates whether or not the sites were sampled in both 2007 ans 2012
info.0712 = info.0712 %>% mutate(resampled = NA)

for (i in 1:nrow(info.0712)) {
  if (info.0712$siteid_07[i] %in% common.sites$siteid_07) {
    info.0712$resampled[i] = 1
  } else {
    info.0712$resampled[i] = 0
  }
}




# Correctly order observations 
info.0712 = info.0712 %>% arrange(site_id, year, visit_no) 

# Assign adequate class to site_id
info.0712$site_id = as.factor(info.0712$site_id)
info.0712$siteid_07 = as.factor(info.0712$siteid_07)
info.0712$siteid_12 = as.factor(info.0712$siteid_12)




# Export the processed data set
write.table(info.0712,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/info_0712.tsv",
            sep = "\t")




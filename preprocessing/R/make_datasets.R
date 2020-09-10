### Francis Banville - Université de Montréal
### September 19th 2019

### The interim data sets, obtained from OpenRefine, are here tidied and merged to form processed data sets
### The sections of our script first refer to the indicators identified by the NARS 
### They then refer to the merging of data sets and computation of data further used in analysis 

### The numbers in the data frame names represent the year of the sampling events
### For example, profile.07 is the data frame of profiles sampled in 2007
### profile.12 is the data frame of profiles sampled in 2012


# Libraries ====
library("dplyr")
library("lubridate")
library("rLakeAnalyzer")
library("stringr")
library("tidyr")
library("vegan")
library("zoo")




#### 1. Water chemisty (profiles) ====


#### 1.1 Tidy the profile.07 data set 


# Import the interim data sets 
profile.07 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/water_chemistry/nla2007_profile_20091008.tsv", header = TRUE,  sep = '\t')

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
  
  # observations on the same site and visit no are in the same layer as the one above them
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
profile.12 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/water_chemistry/nla2012_wide_profile_08232016.tsv", header = TRUE,  sep = '\t')


# Rename some variables 
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


# Change variables class
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







#### 2. Site information ====


#### 2.1 Tidy the info.07 data set 


# Import the interim data set 
info.07 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/site_information/nla2007_sampledlakeinformation_20091113.tsv", header = TRUE,  sep = '\t')


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
info.12 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/site_information/nla2012_wide_siteinfo_08232016.tsv", header = TRUE,  sep = '\t', quote = "\\")


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






#### 3. Secchi ====


#### 3.1 Tidy the secchi.07 data set 

# Import the interim data set 
secchi.07 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/secchi/nla2007_secchi_20091008.tsv", header = TRUE,  sep = '\t', quote = "\\")


# Rename some variables 
colnames(secchi.07) = tolower(colnames(secchi.07)) # variable names in lowercase
names(secchi.07)[names(secchi.07) == "secmean_m"] = "secchi_m"
names(secchi.07)[names(secchi.07) == "site_id"] = "siteid_07"

 
# Change the variable's class
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
secchi.12 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/secchi/nla2012_secchi_08232016.tsv", header = TRUE,  sep = '\t', quote = "\\")

# Rename some variables
colnames(secchi.12) = tolower(colnames(secchi.12)) # variable names in lowercase
names(secchi.12)[names(secchi.12) == "secchi"] = "secchi_m"
names(secchi.12)[names(secchi.12) == "site_id"] = "siteid_12"


# Change the variable's class
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











#### 4. Landscape data ====


#### 4.1 Tidy the landscape.07 data set 

# Import the interim data set 
landscape.07 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/landscape_data/nla2007_basin_landuse_metrics_20061022.tsv", header = TRUE,  sep = '\t', quote = "\\")


# Rename some variables 
colnames(landscape.07) = tolower(colnames(landscape.07)) # variable names in lowercase
names(landscape.07)[names(landscape.07) == "pct_forest_bsn"] = "pct_forest" # % forested basin area
names(landscape.07)[names(landscape.07) == "pct_agric_bsn"] = "pct_agric" # % agriculture basin area
names(landscape.07)[names(landscape.07) == "site_id"] = "siteid_07" 


# Select useful variables
landscape.07 = landscape.07 %>% 
  select(siteid_07, basinarea_km2, pct_forest, pct_agric) %>%
  mutate(year = 2007) # add a year variable 


# Change the variables' class
landscape.07 = landscape.07 %>% mutate(
  siteid_07 = as.factor(siteid_07), 
  year = as.numeric(year),  
  basinarea_km2 = as.numeric(basinarea_km2), 
  pct_forest = as.numeric(pct_forest), 
  pct_agric = as.numeric(pct_agric)) 








#### 4.2 Tidy the landscape.12 data set 


# Import the interim data set 
landscape.12 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/landscape_data/nla2012_wide_watershed.tsv", header = TRUE,  sep = '\t', quote = "\\")

# Rename some variables 
colnames(landscape.12) = tolower(colnames(landscape.12)) # variable names in lowercase
names(landscape.12)[names(landscape.12) == "nlcd2001_forestpct_bsn"] = "pct_forest" # % forested basin area
names(landscape.12)[names(landscape.12) == "nlcd2006_agricpct_bsn"] = "pct_agric" # % agriculture basin area
names(landscape.12)[names(landscape.12) == "site_id"] = "siteid_12" 



# Select useful variables
landscape.12 = landscape.12 %>% 
  select(siteid_12, 
         nlcd2001_11pct_bsn,  nlcd2001_11area_bsn,
         pct_forest, pct_agric) %>%
  mutate(year = 2012) # add a year variable


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









#### 5. Water chemistry (water quality) ====


#### 5.1 Tidy the chemistry.07 data set 

# Import the interim data set 
chemistry.07 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/water_chemistry/NLA2007_WaterQuality_20091123.tsv", header = TRUE,  sep = '\t', quote = "\\")


# Rename some variables
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
    chemistry.07$nutrient_color[i] = "blue" # Blue if TP <= 30 and color <= 20 
  } else if (TP > 30 & color <= 20) {
    chemistry.07$nutrient_color[i] = "green" # Green if TP > 30 and color <= 20
  } else if (TP <= 30 & color > 20) {
    chemistry.07$nutrient_color[i] = "brown" # Brown if TP <= 30 and color > 20
  } else if (TP > 30 & color > 20) {
    chemistry.07$nutrient_color[i] = "murky" # Murky if TP > 30 and color > 20
  } 
}





#### 5.2 Tidy the chemistry.12 data set 

# Import the interim data set 
chemistry.12 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/water_chemistry/nla2012_waterchem_wide.tsv", header = TRUE,  sep = '\t', quote = "\\")

# Rename some variables
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
chla.12 = read.table("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/interim/chlorophyll-a/nla2012_chla_wide.tsv", header = TRUE,  sep = '\t', quote = "\\")

# Rename some variables
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







#### 6. Climate ====


#### 6.1 Climate data from the NOAA 


# Import the raw data sets
# state monthly precipitation (124 years)
precip.124y = read.csv("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/raw/climate_noaa/110-pcp.csv", header = TRUE, skip = 3)
# state montly average temperature (124 years)
avgtemp.124y = read.csv("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/raw/climate_noaa/110-tavg.csv", header = TRUE, skip = 3)
# state montly minimum temperature (124 years)
mintemp.124y = read.csv("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/raw/climate_noaa/110-tmin.csv", header = TRUE, skip = 3)
# state montly maximum temperature (124 years)
maxtemp.124y = read.csv("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/raw/climate_noaa/110-tmax.csv", header = TRUE, skip = 3)



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
states = read.csv("C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/raw/climate_noaa/states.csv", header = TRUE)

names(states)[names(states) == "State"] = "Location" # State name
names(states)[names(states) == "Abbreviation"] = "state" # State abbreviation

climate.0712 = left_join(climate.0712, states, by = "Location") %>%
  select(-Location)









#### 7 Merged data sets ====


# Identify sites sampled in both 2007 and 2012
common.sites = inner_join(info.07, info.12, by = "siteid_07") %>%
  select(siteid_07, siteid_12) %>%
  distinct(siteid_07, siteid_12, .keep_all = TRUE)
dim(common.sites) # 401 repeated sites




#### 7.1  Merged profile data sets 


# Merge profiles data set
profile.0712 = bind_rows(profile.07, profile.12)


# The variable 'resampled' indicates whether or not the sites were sampled in both 2007 ans 2012
profile.0712 = profile.0712 %>% mutate(resampled = NA)

# A site is considered resampled if it is found in the "common.sites" vector 
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



#### Correction of mistakes in the data 

# Some mistakes were indroduced in the data 
# For the sampling event "NLA06608-1868-2012-2", at a depth of 4.02 m, the T is 3075 oC
# It should be 30,75 oC
# For the other events where temp > 60, we can't assume any realistic observations

profile.0712$temp[which(profile.0712$temp > 3075)] = 30.75
profile.0712$temp[which(profile.0712$temp > 60)] = NA


# Export the processed data set
write.table(profile.0712,
            file = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/profile_0712.tsv",
            sep = "\t")






#### 7.2 Merged site information


# Join site information with Secchi depths, landscape and chemistry data 

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




#### Correction of data 


# Correction of inconsistencies between levels of lake_origin
info.0712$lake_origin = str_replace_all(info.0712$lake_origin, "_", "-")



# Complete missing siteid_12 values when sampled in 2007
for (i in 1:nrow(info.0712)) {
  if (info.0712$siteid_07[i] %in% common.sites$siteid_07) {
    k = which(common.sites$siteid_07 == info.0712$siteid_07[i])
    info.0712$siteid_12[i] = as.character(common.sites$siteid_12[k])
  } 
}


# The variable 'resampled' indicates whether or not the sites were sampled in both 2007 ans 2012
# A site is considered resampled if it is found in the "common.sites" vector 
info.0712 = info.0712 %>% mutate(resampled = NA)

for (i in 1:nrow(info.0712)) {
  if (info.0712$siteid_07[i] %in% common.sites$siteid_07) {
    info.0712$resampled[i] = 1
  } else {
    info.0712$resampled[i] = 0
  }
}




# Julian day ====

info.0712$Julian.day = NA # creation of the Julian day variable

for (i in 1:nrow(info.0712)) {
  date = as_date(paste(info.0712$year[i], info.0712$month[i], info.0712$day[i], sep = "-")) # date of sampling event i (yyyy-mm-dd)
  info.0712$Julian.day[i] = yday(date) # Julian day of sampling event i 
}




# Mean climatic observations =======

#### Weight matrices are first computed, and represent the fraction of days in each month 150, 120, 90, 60 and 30 days prior to the sampling event
#### States monthly averaged climatic variables are multiplied by those weights and then summed


#### Compute the mean climatic observations over a 150 days period (5 months) prior to the sampling event

info.0712 = info.0712 %>% mutate(precip_5 = NA, avgtemp_5 = NA, mintemp_5 = NA, maxtemp_5 = NA) # empty variables


climate.W5 = matrix(0, nrow(info.0712), 12) # montly ponderation matrix

# Column and row names of the monthly ponderation matrix 
colnames(climate.W5) = c("jan", "feb", "mar", "apr", 
                          "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
rownames(climate.W5) = info.0712$sampling_event

climate.W5 = as.data.frame(climate.W5) # the monthly ponderation matrix as a data frame



for (i in 1:nrow(info.0712)) { # Each row represent a sampling event 
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
  {climate.W5$jan[i] = filter(pond.in.month, month == "janv.")$Freq}
 
  if(nrow(filter(pond.in.month, month == "févr.")) != 0)
  {climate.W5$feb[i] = filter(pond.in.month, month == "févr.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "mars")) != 0)
  {climate.W5$mar[i] = filter(pond.in.month, month == "mars")$Freq}
  
  if(nrow(filter(pond.in.month, month == "avr.")) != 0)
  {climate.W5$apr[i] = filter(pond.in.month, month == "avr.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "mai")) != 0)
  {climate.W5$may[i] = filter(pond.in.month, month == "mai")$Freq}
  
  if(nrow(filter(pond.in.month, month == "juin")) != 0)
  {climate.W5$jun[i] = filter(pond.in.month, month == "juin")$Freq}
  
  if(nrow(filter(pond.in.month, month == "juil.")) != 0)
  {climate.W5$jul[i] = filter(pond.in.month, month == "juil.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "août")) != 0)
  {climate.W5$aug[i] = filter(pond.in.month, month == "août")$Freq}
  
  if(nrow(filter(pond.in.month, month == "sept.")) != 0)
  {climate.W5$sep[i] = filter(pond.in.month, month == "sept.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "oct.")) != 0)
  {climate.W5$oct[i] = filter(pond.in.month, month == "oct.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "nov.")) != 0)
  {climate.W5$nov[i] = filter(pond.in.month, month == "nov.")$Freq}
  
  if(nrow(filter(pond.in.month, month == "déc.")) != 0)
  {climate.W5$dec[i] = filter(pond.in.month, month == "déc.")$Freq}
}

}


climate.W5 = decostand(climate.W5, "total", MARGIN = 1) # proportion of the 150 days period



# Mean climate metrics for 150 days before each sampling event 
# Each row represent a sampling event 
for (i in 1:nrow(info.0712)) {
  state.i = as.character(info.0712$state[i])
  year.i = info.0712$year[i]
  
  climate.i = climate.0712 %>% filter(state == state.i, year == year.i)
  climate.W.i = t(climate.W5[i,])
  
  info.0712$precip_5[i] = sum(climate.i$precip_mm * climate.W.i)
  info.0712$avgtemp_5[i] = sum(climate.i$avgtemp_C * climate.W.i)
  info.0712$mintemp_5[i] = sum(climate.i$mintemp_C * climate.W.i)
  info.0712$maxtemp_5[i] = sum(climate.i$maxtemp_C * climate.W.i)
}







#### Compute the mean climatic observations over a 120 days period (4 months) prior to the sampling event

info.0712 = info.0712 %>% mutate(precip_4 = NA, avgtemp_4 = NA, mintemp_4 = NA, maxtemp_4 = NA)

climate.W4 = matrix(0, nrow(info.0712), 12) # montly ponderation matrix
colnames(climate.W4) = c("jan", "feb", "mar", "apr", 
                         "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
rownames(climate.W4) = info.0712$sampling_event
climate.W4 = as.data.frame(climate.W4)



for (i in 1:nrow(info.0712)) {
  year = info.0712$year[i]
  month = info.0712$month[i]
  day = info.0712$day[i]
  
  if (!is.na(year) & !is.na(month) & !is.na(day)) {
    
    date.sampled = date(paste(year, month, day, sep = "-")) # date of sampling event
    date.120d = date.sampled - 119 # date 120 days before
    
    pond.in.month = as.data.frame(table(as.yearmon(seq(date.120d, date.sampled, "day")))) %>% # nb of days in each month between the 2 dates 
      separate(Var1, into = c("month", "year"), sep = " ") %>%
      select(-year)
    
    # Nb of days of each month inside climate.W matrix
    
    if(nrow(filter(pond.in.month, month == "janv.")) != 0)
    {climate.W4$jan[i] = filter(pond.in.month, month == "janv.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "févr.")) != 0)
    {climate.W4$feb[i] = filter(pond.in.month, month == "févr.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "mars")) != 0)
    {climate.W4$mar[i] = filter(pond.in.month, month == "mars")$Freq}
    
    if(nrow(filter(pond.in.month, month == "avr.")) != 0)
    {climate.W4$apr[i] = filter(pond.in.month, month == "avr.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "mai")) != 0)
    {climate.W4$may[i] = filter(pond.in.month, month == "mai")$Freq}
    
    if(nrow(filter(pond.in.month, month == "juin")) != 0)
    {climate.W4$jun[i] = filter(pond.in.month, month == "juin")$Freq}
    
    if(nrow(filter(pond.in.month, month == "juil.")) != 0)
    {climate.W4$jul[i] = filter(pond.in.month, month == "juil.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "août")) != 0)
    {climate.W4$aug[i] = filter(pond.in.month, month == "août")$Freq}
    
    if(nrow(filter(pond.in.month, month == "sept.")) != 0)
    {climate.W4$sep[i] = filter(pond.in.month, month == "sept.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "oct.")) != 0)
    {climate.W4$oct[i] = filter(pond.in.month, month == "oct.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "nov.")) != 0)
    {climate.W4$nov[i] = filter(pond.in.month, month == "nov.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "déc.")) != 0)
    {climate.W4$dec[i] = filter(pond.in.month, month == "déc.")$Freq}
  }
  
}


climate.W4 = decostand(climate.W4, "total", MARGIN = 1) # proportion of the 120 days period



# Mean climate metrics for 120 days before each sampling event 
# Each row represent a sampling event 
for (i in 1:nrow(info.0712)) {
  state.i = as.character(info.0712$state[i])
  year.i = info.0712$year[i]
  
  climate.i = climate.0712 %>% filter(state == state.i, year == year.i)
  climate.W.i = t(climate.W4[i,])
  
  info.0712$precip_4[i] = sum(climate.i$precip_mm * climate.W.i)
  info.0712$avgtemp_4[i] = sum(climate.i$avgtemp_C * climate.W.i)
  info.0712$mintemp_4[i] = sum(climate.i$mintemp_C * climate.W.i)
  info.0712$maxtemp_4[i] = sum(climate.i$maxtemp_C * climate.W.i)
}









#### Compute the mean climatic observations over a 90 days period (3 months) prior to the sampling event

info.0712 = info.0712 %>% mutate(precip_3 = NA, avgtemp_3 = NA, mintemp_3 = NA, maxtemp_3 = NA)

climate.W3 = matrix(0, nrow(info.0712), 12) # montly ponderation matrix
colnames(climate.W3) = c("jan", "feb", "mar", "apr", 
                         "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
rownames(climate.W3) = info.0712$sampling_event
climate.W3 = as.data.frame(climate.W3)



for (i in 1:nrow(info.0712)) {
  year = info.0712$year[i]
  month = info.0712$month[i]
  day = info.0712$day[i]
  
  if (!is.na(year) & !is.na(month) & !is.na(day)) {
    
    date.sampled = date(paste(year, month, day, sep = "-")) # date of sampling event
    date.90d = date.sampled - 89 # date 90 days before
    
    pond.in.month = as.data.frame(table(as.yearmon(seq(date.90d, date.sampled, "day")))) %>% # nb of days in each month between the 2 dates 
      separate(Var1, into = c("month", "year"), sep = " ") %>%
      select(-year)
    
    # Nb of days of each month inside climate.W matrix
    
    if(nrow(filter(pond.in.month, month == "janv.")) != 0)
    {climate.W3$jan[i] = filter(pond.in.month, month == "janv.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "févr.")) != 0)
    {climate.W3$feb[i] = filter(pond.in.month, month == "févr.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "mars")) != 0)
    {climate.W3$mar[i] = filter(pond.in.month, month == "mars")$Freq}
    
    if(nrow(filter(pond.in.month, month == "avr.")) != 0)
    {climate.W3$apr[i] = filter(pond.in.month, month == "avr.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "mai")) != 0)
    {climate.W3$may[i] = filter(pond.in.month, month == "mai")$Freq}
    
    if(nrow(filter(pond.in.month, month == "juin")) != 0)
    {climate.W3$jun[i] = filter(pond.in.month, month == "juin")$Freq}
    
    if(nrow(filter(pond.in.month, month == "juil.")) != 0)
    {climate.W3$jul[i] = filter(pond.in.month, month == "juil.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "août")) != 0)
    {climate.W3$aug[i] = filter(pond.in.month, month == "août")$Freq}
    
    if(nrow(filter(pond.in.month, month == "sept.")) != 0)
    {climate.W3$sep[i] = filter(pond.in.month, month == "sept.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "oct.")) != 0)
    {climate.W4$oct[i] = filter(pond.in.month, month == "oct.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "nov.")) != 0)
    {climate.W3$nov[i] = filter(pond.in.month, month == "nov.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "déc.")) != 0)
    {climate.W3$dec[i] = filter(pond.in.month, month == "déc.")$Freq}
  }
  
}


climate.W3 = decostand(climate.W3, "total", MARGIN = 1) # proportion of the 90 days period



# Mean climate metrics for 90 days before each sampling event 
# Each row represent a sampling event 
for (i in 1:nrow(info.0712)) {
  state.i = as.character(info.0712$state[i])
  year.i = info.0712$year[i]
  
  climate.i = climate.0712 %>% filter(state == state.i, year == year.i)
  climate.W.i = t(climate.W3[i,])
  
  info.0712$precip_3[i] = sum(climate.i$precip_mm * climate.W.i)
  info.0712$avgtemp_3[i] = sum(climate.i$avgtemp_C * climate.W.i)
  info.0712$mintemp_3[i] = sum(climate.i$mintemp_C * climate.W.i)
  info.0712$maxtemp_3[i] = sum(climate.i$maxtemp_C * climate.W.i)
}








#### Compute the mean climatic observations over a 60 days period (2 months) prior to the sampling event

info.0712 = info.0712 %>% mutate(precip_2 = NA, avgtemp_2 = NA, mintemp_2 = NA, maxtemp_2 = NA)

climate.W2 = matrix(0, nrow(info.0712), 12) # montly ponderation matrix
colnames(climate.W2) = c("jan", "feb", "mar", "apr", 
                         "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
rownames(climate.W2) = info.0712$sampling_event
climate.W2 = as.data.frame(climate.W2)



for (i in 1:nrow(info.0712)) {
  year = info.0712$year[i]
  month = info.0712$month[i]
  day = info.0712$day[i]
  
  if (!is.na(year) & !is.na(month) & !is.na(day)) {
    
    date.sampled = date(paste(year, month, day, sep = "-")) # date of sampling event
    date.60d = date.sampled - 59 # date 60 days before
    
    pond.in.month = as.data.frame(table(as.yearmon(seq(date.60d, date.sampled, "day")))) %>% # nb of days in each month between the 2 dates 
      separate(Var1, into = c("month", "year"), sep = " ") %>%
      select(-year)
    
    # Nb of days of each month inside climate.W matrix
    
    if(nrow(filter(pond.in.month, month == "janv.")) != 0)
    {climate.W2$jan[i] = filter(pond.in.month, month == "janv.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "févr.")) != 0)
    {climate.W2$feb[i] = filter(pond.in.month, month == "févr.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "mars")) != 0)
    {climate.W2$mar[i] = filter(pond.in.month, month == "mars")$Freq}
    
    if(nrow(filter(pond.in.month, month == "avr.")) != 0)
    {climate.W2$apr[i] = filter(pond.in.month, month == "avr.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "mai")) != 0)
    {climate.W2$may[i] = filter(pond.in.month, month == "mai")$Freq}
    
    if(nrow(filter(pond.in.month, month == "juin")) != 0)
    {climate.W2$jun[i] = filter(pond.in.month, month == "juin")$Freq}
    
    if(nrow(filter(pond.in.month, month == "juil.")) != 0)
    {climate.W2$jul[i] = filter(pond.in.month, month == "juil.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "août")) != 0)
    {climate.W2$aug[i] = filter(pond.in.month, month == "août")$Freq}
    
    if(nrow(filter(pond.in.month, month == "sept.")) != 0)
    {climate.W2$sep[i] = filter(pond.in.month, month == "sept.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "oct.")) != 0)
    {climate.W2$oct[i] = filter(pond.in.month, month == "oct.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "nov.")) != 0)
    {climate.W2$nov[i] = filter(pond.in.month, month == "nov.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "déc.")) != 0)
    {climate.W2$dec[i] = filter(pond.in.month, month == "déc.")$Freq}
  }
  
}


climate.W2 = decostand(climate.W2, "total", MARGIN = 1) # proportion of the 60 days period



# Mean climate metrics for 60 days before each sampling event 
# Each row represent a sampling event 
for (i in 1:nrow(info.0712)) {
  state.i = as.character(info.0712$state[i])
  year.i = info.0712$year[i]
  
  climate.i = climate.0712 %>% filter(state == state.i, year == year.i)
  climate.W.i = t(climate.W2[i,])
  
  info.0712$precip_2[i] = sum(climate.i$precip_mm * climate.W.i)
  info.0712$avgtemp_2[i] = sum(climate.i$avgtemp_C * climate.W.i)
  info.0712$mintemp_2[i] = sum(climate.i$mintemp_C * climate.W.i)
  info.0712$maxtemp_2[i] = sum(climate.i$maxtemp_C * climate.W.i)
}











#### Compute the mean climatic observations over a 30 days period (1 month) prior to the sampling event

info.0712 = info.0712 %>% mutate(precip_1 = NA, avgtemp_1 = NA, mintemp_1 = NA, maxtemp_1 = NA)

climate.W1 = matrix(0, nrow(info.0712), 12) # montly ponderation matrix
colnames(climate.W1) = c("jan", "feb", "mar", "apr", 
                         "may", "jun", "jul", "aug", "sep", "oct", "nov", "dec")
rownames(climate.W1) = info.0712$sampling_event
climate.W1 = as.data.frame(climate.W1)



for (i in 1:nrow(info.0712)) {
  year = info.0712$year[i]
  month = info.0712$month[i]
  day = info.0712$day[i]
  
  if (!is.na(year) & !is.na(month) & !is.na(day)) {
    
    date.sampled = date(paste(year, month, day, sep = "-")) # date of sampling event
    date.30d = date.sampled - 29 # date 30 days before
    
    pond.in.month = as.data.frame(table(as.yearmon(seq(date.30d, date.sampled, "day")))) %>% # nb of days in each month between the 2 dates 
      separate(Var1, into = c("month", "year"), sep = " ") %>%
      select(-year)
    
    # Nb of days of each month inside climate.W matrix
    
    if(nrow(filter(pond.in.month, month == "janv.")) != 0)
    {climate.W1$jan[i] = filter(pond.in.month, month == "janv.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "févr.")) != 0)
    {climate.W1$feb[i] = filter(pond.in.month, month == "févr.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "mars")) != 0)
    {climate.W1$mar[i] = filter(pond.in.month, month == "mars")$Freq}
    
    if(nrow(filter(pond.in.month, month == "avr.")) != 0)
    {climate.W1$apr[i] = filter(pond.in.month, month == "avr.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "mai")) != 0)
    {climate.W1$may[i] = filter(pond.in.month, month == "mai")$Freq}
    
    if(nrow(filter(pond.in.month, month == "juin")) != 0)
    {climate.W1$jun[i] = filter(pond.in.month, month == "juin")$Freq}
    
    if(nrow(filter(pond.in.month, month == "juil.")) != 0)
    {climate.W1$jul[i] = filter(pond.in.month, month == "juil.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "août")) != 0)
    {climate.W1$aug[i] = filter(pond.in.month, month == "août")$Freq}
    
    if(nrow(filter(pond.in.month, month == "sept.")) != 0)
    {climate.W1$sep[i] = filter(pond.in.month, month == "sept.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "oct.")) != 0)
    {climate.W1$oct[i] = filter(pond.in.month, month == "oct.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "nov.")) != 0)
    {climate.W2$nov[i] = filter(pond.in.month, month == "nov.")$Freq}
    
    if(nrow(filter(pond.in.month, month == "déc.")) != 0)
    {climate.W1$dec[i] = filter(pond.in.month, month == "déc.")$Freq}
  }
  
}


climate.W1 = decostand(climate.W1, "total", MARGIN = 1) # proportion of the 60 days period



# Mean climate metrics for 30 days before each sampling event 
# Each row represent a sampling event 
for (i in 1:nrow(info.0712)) {
  state.i = as.character(info.0712$state[i])
  year.i = info.0712$year[i]
  
  climate.i = climate.0712 %>% filter(state == state.i, year == year.i)
  climate.W.i = t(climate.W1[i,])
  
  info.0712$precip_1[i] = sum(climate.i$precip_mm * climate.W.i)
  info.0712$avgtemp_1[i] = sum(climate.i$avgtemp_C * climate.W.i)
  info.0712$mintemp_1[i] = sum(climate.i$mintemp_C * climate.W.i)
  info.0712$maxtemp_1[i] = sum(climate.i$maxtemp_C * climate.W.i)
}





# Correctly order observations 
info.0712 = info.0712 %>% arrange(site_id, year, visit_no) 


# Assign adequate class to site_id
info.0712$site_id = as.factor(info.0712$site_id)
info.0712$siteid_07 = as.factor(info.0712$siteid_07)
info.0712$siteid_12 = as.factor(info.0712$siteid_12)



# Export the processed data set
write.table(info.0712,
            file = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/info_0712.tsv",
            sep = "\t")





#### RLA1. meta.depths()  ====


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


# When the top and bottom of the metalimnion are at the same depth, we considered that the metalimnion 
# thickness was very small and we thus only identified the epilimnion and hypolimnion of those lakes
for (i in 1:nrow(profile.0712)) { # Each row represent a depth of a sampling event
  sampling.event.i = profile.0712$sampling_event[i]
  depth = profile.0712$depth[i]
  
  k = which(top.bottom.meta$sampling_event == sampling.event.i) # Identify the sampling event in the top.bottom.meta matrix
  
  top.depth = top.bottom.meta$top_depth[k]
  bottom.depth = top.bottom.meta$bottom_depth[k]
  
  if (length(k) == 0) {
    profile.0712$layer_r[i] = "E"
  } else if (is.na(top.depth) | is.na(bottom.depth)) { # If there is no metalimnion in the sampling event, the sample is in the epilimnion
    profile.0712$layer_r[i] = "E"
  } else if (depth < top.depth) { # If the sample is above the top of the metalimnion, it is in the epilimnion
    profile.0712$layer_r[i] = "E"
  } else if (depth >= top.depth & depth <= bottom.depth) { # If the sample is in between the top and bottom of the metalimnion, it is in the metalimnion
    profile.0712$layer_r[i] = "M"
  } else if (depth > bottom.depth) { # If the sample is below the bottom of the metalimnion, it is in the hypolimnion
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
different / total # 66,16 %






##### RLA2. Lake types according to their layers ====

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



# Assign a type to all sampling events according to their layers

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



# Simplified lake type
# If the lake type is either 3, 4 or 6, it is considered abnormal
# Those types are regrouped under the type "other" (new 4)
# Lake type # 5 are renamed # 3

info.0712$type_simple = NA
for (i in 1:nrow(info.0712)) {
  if(!is.na(info.0712$type[i])) {
  if(info.0712$type[i] %in% c(3, 4, 6)) {
    info.0712$type_simple[i] = 4
  } else if (info.0712$type[i] == 5) {
    info.0712$type_simple[i] = 3
  } else {
    info.0712$type_simple[i] = info.0712$type[i]
  }
 }
}



# Lake stratification 
# The lake is stratified if it is of type 1, 2, 3 or 4 

for (i in 1:nrow(info.0712)) {
  if(info.0712$type[i] %in% 1:4) {
    info.0712$stratified[i] = 1
  } else {
    info.0712$stratified[i] = 0
  }
}






#### RLA3. approx.bathy  (cone method)  ####

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

# Remove observations where max sampled depth = 0
max.sampled.observed.depth = max.sampled.observed.depth %>%
  filter(max.sampled != 0)


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
  
  if(!is.na(max.depth.lake.area$max.sampled[i]) &  !is.na(max.depth.lake.area$lake.area[i])) {
    hypsography.curves[[i]] = approx.bathy(Zmax = max.depth.lake.area$max.sampled[i],
                                           lkeArea = max.depth.lake.area$lake.area[i],
                                           method = "cone", 
                                           zinterval = 0.1)
    
    hypsography.curves[[i]]$sampling_event = max.depth.lake.area$sampling_event[i]
    
  }
}


# Unlist the hypsography curves 
hypsography.curves = Reduce(bind_rows, hypsography.curves) %>%
  select(sampling_event, depths, Area.at.z)







#### RLA4. Lake metrics ====


# Data frame of lake metrics
nvar = 4 # Number of colomns of the data frame
lake.metrics = data.frame(matrix(nrow = length(unique(info.0712$sampling_event)), ncol = nvar))

# Columns names
colnames(lake.metrics) = c("sampling_event", "epitemp_C", "metatemp_C", "hypotemp_C")


# Sampling events inside data frame
lake.metrics$sampling_event = unique(info.0712$sampling_event)



##### RLA4.1 Volumetrically averaged layer temp ====


#### Volumetrically averaged epilimnion temp 

for (i in 1:nrow(lake.metrics)) {
  
  sampling.event = lake.metrics$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp))  # temperature profile
  
  wtr = wtr.depths$temp # temperature
  depths = wtr.depths$depth # depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event) # area profile
  
  bthA = bthA.bthD$Area.at.z # area at the depth bethD
  bthD = bthA.bthD$depths # depth
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton 
    
    lake.metrics$epitemp_C[i] = epi.temperature(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD) # computation of the volumnetrically averaged epi temp
  }
  
}



#### Volumetrically averaged metalimnion temp 


meta.temperature <- function(wtr, depths, bthA, bthD){ # function to computer the averaged metalimnion temp 
  
  md = rLakeAnalyzer::meta.depths(wtr, depths) # top and bottom of the metalimnion
  
  if(is.na(md[1])){
    avg_temp = NA
  }else{
    avg_temp = layer.temperature(md[1],md[2], wtr=wtr, depths=depths, bthA=bthA, bthD=bthD) # averaged temp between the top and bottom of the metalimnion
  }
  return(avg_temp)
}



for (i in 1:nrow(lake.metrics)){
  
  sampling.event = lake.metrics$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) # temperature profile
  
  wtr = wtr.depths$temp # temperature
  depths = wtr.depths$depth # depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event) # hypsography curves
  
  bthA = bthA.bthD$Area.at.z # area at the depth bthD
  bthD = bthA.bthD$depths # depth
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    lake.metrics$metatemp_C[i] = meta.temperature(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD) # computation of the volumetrically averaged meta temp
  }
}





#### Volumetrically averaged hypolimnion temp 

for (i in 1:nrow(lake.metrics)) {
  
  sampling.event = lake.metrics$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp))# temperature profile
  
  wtr = wtr.depths$temp # temperature
  depths = wtr.depths$depth # depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event) # hypsography curves
  
  bthA = bthA.bthD$Area.at.z # area at the depth bthD
  bthD = bthA.bthD$depths # depth
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    lake.metrics$hypotemp_C[i] = hypo.temperature(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD) # computation of the volumetrically averaged hypo temp
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






#### RLA4.2 Epilimnion thickness ====

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





##### RLA4.3 Average layer density ====


layer.dens= data.frame(matrix(nrow = length(unique(info.0712$sampling_event)), ncol = 4)) # empty data frame

colnames(layer.dens) = c("sampling_event", "epidens_kgm3", "metadens_kgm3", "hypodens_kgm3") # names of the empty data frame


# Sampling events inside data frame
layer.dens$sampling_event = unique(info.0712$sampling_event)


#### Epilimnion density 

epi.density <- function(wtr, depths, bthA, bthD){ # function to computer the density of the epilimnion  
  
  md = rLakeAnalyzer::meta.depths(wtr, depths) # top and bottom of the metalimnion
  
  if(is.na(md[1])){
    dens = layer.density(0,max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD)
  }else{
    dens = layer.density(0,md[1], wtr=wtr, depths=depths, bthA=bthA, bthD=bthD) # density between 0 depth and top of metalimnion
  }
  return(dens)
}



for (i in 1:length(unique(info.0712$sampling_event))) {
  
  sampling.event = layer.dens$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) # temperature profile
  
  wtr = wtr.depths$temp # temperature
  depths = wtr.depths$depth # depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event) # hypsography curves
  
  bthA = bthA.bthD$Area.at.z # area at the depth bthD
  bthD = bthA.bthD$depths # depth
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    layer.dens$epidens_kgm3[i] = epi.density(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD) # density of the epilimnion
  }
}








#### Metalimnion density 

meta.density <- function(wtr, depths, bthA, bthD){ # function to computer the density of the metalimnion  
  
  md = rLakeAnalyzer::meta.depths(wtr, depths) # top and bottom of the metalimnion
  
  if(is.na(md[1])){
    dens = NA
  }else{
    dens = layer.density(md[1],md[2], wtr=wtr, depths=depths, bthA=bthA, bthD=bthD) # density between the top and bottom of the metalimnion
  }
  return(dens)
}



for (i in 1:length(unique(info.0712$sampling_event))) {
  
  sampling.event = layer.dens$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) # temperature profile
  
  wtr = wtr.depths$temp # temperature
  depths = wtr.depths$depth # depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event) # hypsography curves
  
  bthA = bthA.bthD$Area.at.z # area at the depth bthD
  bthD = bthA.bthD$depths # depth
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    layer.dens$metadens_kgm3[i] = meta.density(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD) # density of the metalimnion
  }
}









#### Hypolimnion density 

hypo.density <- function(wtr, depths, bthA, bthD){ # function to computer the density of the hypolimnion 
  
  md = rLakeAnalyzer::meta.depths(wtr, depths) # top and bottom of the metalimnion
  
  if(is.na(md[2])){
    dens = NA
  }else{
    dens = layer.density(md[2],max(depths), wtr=wtr, depths=depths, bthA=bthA, bthD=bthD) # density between the bottom of the metalimnion and the maximum depth
  }
  return(dens)
}



for (i in 1:length(unique(info.0712$sampling_event))) {
  
  sampling.event = layer.dens$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) # temperature profile
  
  wtr = wtr.depths$temp # temperature
  depths = wtr.depths$depth # depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event) # hypsography curves
  
  bthA = bthA.bthD$Area.at.z # area at the depth bthD
  bthD = bthA.bthD$depths # depth
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    layer.dens$hypodens_kgm3[i] = hypo.density(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD) # computation of the density of the hypolimnion
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






#### RLA4.4 Internal energy ====


# Computation of the lake internal energy (J)

for (i in 1:length(unique(info.0712$sampling_event))) {
  
  sampling.event = info.0712$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) # temperature profile
  
  wtr = wtr.depths$temp # temperature
  depths = wtr.depths$depth # profile
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event) # hypsography curves
  
  bthA = bthA.bthD$Area.at.z # area at the depth bthD
  bthD = bthA.bthD$depths # depth
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton   
    
    info.0712$intE_Jm2[i] = internal.energy(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD) # computation of the lake internal energy
  }
}





#### RLA4.5 Thermocline depth ====


# Thermocline depth (m) for each sampling event
thermodepth = profile.0712 %>% group_by(sampling_event) %>%
  summarize(thermodepth_m = thermo.depth(wtr = temp, depths = depth)) 

# Inclusion of thermocline depth into info.0712 data set 
info.0712 = left_join(info.0712, thermodepth, by = "sampling_event")





#### RLA4.6 Anoxia and hypoxia ====

# Which sampling event are anoxic somewhere in their water column
# At which depth those sampling event begins to be anoxic ?
anoxia = profile.0712 %>% filter(DO == 0) %>%
  group_by(sampling_event) %>%
  summarize(anoxia = 1, anoxiadepth_m = min(depth)) # minimum depth at which a lake is anoxic (DO = 0)


# Which sampling event are hypoxic somewhere in their water column
# At which depth those sampling event begins to be hypoxic ?
hypoxia = profile.0712 %>% filter(DO < 2) %>%
  group_by(sampling_event) %>%
  summarize(hypoxia = 1, hypoxiadepth_m = min(depth)) # minimum depth at which a lake is hypoxic (DO < 2)


# Join anoxia and hypoxia data to the info.0712 data set 
info.0712 = left_join(info.0712, anoxia, by = "sampling_event") %>%
  left_join(hypoxia)


# For each sampling event present in the profile.0712 data set, change NA to 0 if
# no anoxia or hypoxia detected
for (i in 1:nrow(info.0712)) {
  sampling.event.i = info.0712$sampling_event[i]
  
  if(sampling.event.i %in% profile.0712$sampling_event &
     is.na(info.0712$anoxia[i])) {    # no anoxia detected in this sampling event 
    info.0712$anoxia[i] = 0
  }
  
  if(sampling.event.i %in% profile.0712$sampling_event &
     is.na(info.0712$hypoxia[i])) {    # no hypoxia detected in this sampling event 
    info.0712$hypoxia[i] = 0
  }
}




#### Estimated lake volumes (method = cone) 
# The volume of a lake is its maximum sampled depth * lake area / 3

info.0712 = info.0712 %>%
  mutate(lakevolume_m3 = (area_km2 * 1000^2) * sampled_depthmax_m / 3) # 1 km2 = 1000^2 m 




#### Anoxia and hypoxia depth ratio 

info.0712 = info.0712 %>%
  mutate(anoxiadepth_pct = 1 - anoxiadepth_m / sampled_depthmax_m,
         hypoxiadepth_pct = 1 - hypoxiadepth_m/sampled_depthmax_m) # fractions of anoxic and hypoxic depths


# If no anoxia or no hypoxia was observed, the anoxia or hypoxia depth are set at 0
for (i in 1:nrow(info.0712)) {
  if (!is.na(info.0712$anoxia[i])) {
    if(info.0712$anoxia[i] == 0) {
      info.0712$anoxiadepth_pct[i] = 0
    }
  }
  if (!is.na(info.0712$hypoxia[i])) {
    if (info.0712$hypoxia[i] == 0) {
      info.0712$hypoxiadepth_pct[i] = 0
    }
  }
}


# Estimated anoxia and hypoxia volumes (method = cone)
# Function of lake volume and fraction of anoxia (or hypoxia) depth

info.0712 = info.0712 %>% 
  mutate(anoxiavolume_m3 = lakevolume_m3 * anoxiadepth_pct ^ 3, 
         hypoxiavolume_m3 = lakevolume_m3 * hypoxiadepth_pct ^ 3)  

# Estimated anoxia and hypoxia volumes (in %)

info.0712 = info.0712 %>%
  mutate(anoxiavolume_pct = anoxiavolume_m3 / lakevolume_m3,
         hypoxiavolume_pct = hypoxiavolume_m3 / lakevolume_m3)





#### RLA4.7 Schmidt stability ====



for (i in 1:nrow(info.0712)) {
  
  sampling.event = info.0712$sampling_event[i]
  
  wtr.depths = profile.0712 %>% 
    filter(sampling_event == sampling.event, !is.na(temp)) # temperature profile
  
  wtr = wtr.depths$temp # temperature
  depths = wtr.depths$depth # depth
  
  bthA.bthD = hypsography.curves %>%
    filter(sampling_event == sampling.event) # hypsography curves
  
  bthA = bthA.bthD$Area.at.z # area at the depth bthD
  bthD = bthA.bthD$depths # depth
  
  if(nrow(wtr.depths) >= 2 & nrow(bthA.bthD) >= 2) { # requirements of the funciton 
    
    info.0712$schmidth.stability_Jm2[i] = schmidt.stability(wtr = wtr, depths = depths, bthA = bthA, bthD = bthD) # computation of the Schmidt stability
  }
  
}










#### Data set ready for analysis ====


# Select response and explanatory variables 
strat.0712 = info.0712 %>%
  # Change variables class
  mutate(sampling_event = sampling_event,  
         site_id = as.factor(site_id), 
         resampled = as.factor(resampled),
         type = as.factor(type),
         type_simple = as.factor(type_simple),
         stratifies = as.factor(stratified),
         deltaT = as.numeric(deltaT_C), 
         epithick = as.numeric(epithick_m),
         thermodepth = as.numeric(thermodepth_m),
         anoxiaV = as.numeric(anoxiavolume_m3),
         hypoxiaV = as.numeric(hypoxiavolume_m3),
         schmidth_stability = as.numeric(schmidth.stability_Jm2),
         month = as.ordered(month),
         year = as.ordered(year),
         Julian_day = as.numeric(Julian.day),
         lat = as.numeric(lat),
         lon = as.numeric(lon),
         elevation = as.factor(elevation_m),
         ECO9 = as.factor(ECO9),
         lake_origin = as.factor(lake_origin),
         area = as.numeric(area_km2),
         volume = as.numeric(lakevolume_m3),
         WALA_ratio = as.numeric(WALA_ratio),
         depth = as.numeric(sampled_depthmax_m),
         SDI = as.numeric(SLD),
         forest = as.numeric(pct_forest),
         agric = as.numeric(pct_agric),
         precip5 = as.numeric(precip_5),
         avgtemp5 = as.numeric(avgtemp_5),
         mintemp5 = as.numeric(mintemp_5),
         maxtemp5 = as.numeric(maxtemp_5),
         precip4 = as.numeric(precip_4),
         avgtemp4 = as.numeric(avgtemp_4),
         mintemp4 = as.numeric(mintemp_4),
         maxtemp4 = as.numeric(maxtemp_4),
         precip3 = as.numeric(precip_3),
         avgtemp3 = as.numeric(avgtemp_3),
         mintemp3 = as.numeric(mintemp_3),
         maxtemp3 = as.numeric(maxtemp_3),
         precip2 = as.numeric(precip_2),
         avgtemp2 = as.numeric(avgtemp_2),
         mintemp2 = as.numeric(mintemp_2),
         maxtemp2 = as.numeric(maxtemp_2),
         precip1 = as.numeric(precip_1),
         avgtemp1 = as.numeric(avgtemp_1),
         mintemp1 = as.numeric(mintemp_1),
         maxtemp1 = as.numeric(maxtemp_1),
         chla = as.numeric(chla_ugL),
         color = as.numeric(color_PCU),
         TN = as.numeric(NTL_ugL),
         TP = as.numeric(PTL_ugL),
         DOC = as.numeric(DOC_mgL),
         cond = as.numeric(cond_uScm),
         turb = as.numeric(turb_NTU),
         nutrient_color = as.factor(nutrient_color)) %>%
  filter(visit_no == 1) %>%  # filter for the first visit no only
  # selection of useful variables
  select(sampling_event, site_id, resampled,
         type, type_simple, stratified, deltaT, epithick, thermodepth, anoxiaV, hypoxiaV, schmidth_stability,
         month, year, Julian_day, lat, lon, elevation, ECO9, lake_origin,
         area, volume, WALA_ratio, depth,
         SDI, forest, agric,
         precip5, avgtemp5, mintemp5, maxtemp5,
         precip4, avgtemp4, mintemp4, maxtemp4,
         precip3, avgtemp3, mintemp3, maxtemp3,
         precip2, avgtemp2, mintemp2, maxtemp2,
         precip1, avgtemp1, mintemp1, maxtemp1,
         chla, color, TN, TP, DOC, cond, turb, nutrient_color)



# Remove the second observation of each sampling event that is present more than one in the data set 
repeated.sampling.event = strat.0712  %>% group_by(sampling_event) %>% count() %>% filter(n != 1)

for (i in 1:nrow(repeated.sampling.event)) {
  strat.0712 = strat.0712[-which(strat.0712$sampling_event %in% repeated.sampling.event$sampling_event[i])[2],]
}


# Sampling events as row names
rownames(strat.0712) = strat.0712$sampling_event

# Remove sampling event from the data set 
strat.0712 = strat.0712 %>% select(-sampling_event)


# Export data set ready for analysis

write.table(strat.0712,
            file = "C:/Users/franc/Documents/Maitrise/Travaux_diriges/US_LakeProfiles/data/processed/strat_0712.tsv",
            sep = "\t")






# Change analysis ====

# Reshaping of the data frame for further analysis 
# Resampled sites only 

info.2007r = strat.0712 %>% filter(resampled == 1, year == 2007) # sampling events in 2007 of resampled sites
info.2012r = strat.0712 %>% filter(resampled == 1, year == 2012) # sampling events in 2012 of resampled sites

info.0712r = left_join(info.2007r, info.2012r, by = "site_id", suffix = c(".07", ".12")) # Combine 2007 and 2012 sampling events





##### END OF CODE

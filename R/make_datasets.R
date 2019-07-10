### Francis Banville - Université de Montréal
### July 10th 2019

### The interim data sets, obtained from OpenRefine, are here tidied and merged to form processed data sets
### The sections of our script first refer to the indicators identified by the NARS 
### Then they represent the merging of data sets and the disctinction between repeated and non repeated sites


# Libraries
library("dplyr")
library("tidyr")




#### 1. Water chemisty (profiles) ####


# Import the interim data sets 
profile.07 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/water_chemistry/nla2007_profile_20091008.tsv", header = TRUE,  sep = '\t')
profile.12 = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/water_chemistry/nla2007_profile_20091008.tsv", header = TRUE,  sep = '\t')


#### 1.1 Tidy the 2007 lake profiles data set ####
nla.2007.profile.2 = nla.2007.profile %>%
  # Observations without site_id or depth values are removed 
  filter(SITE_ID != "", !is.na(DEPTH)) %>%
  # Unuseful variables are also removed
  select(-YEAR, -SAMPLED_PROFILE, -FLAG_PROFILE, -COMMENT_PROFILE) %>%
  # Sort by site_id, visit_no and depth for each site
  arrange(SITE_ID, VISIT_NO, DEPTH)


nla.2007.profile.3 = nla.2007.profile.2 %>%
  # Average observations on same site, visit no and depth 
  group_by(SITE_ID, VISIT_NO, DEPTH) %>%
  summarise(YEAR = YEAR[1], 
            MONTH = MONTH[1], 
            DAY = DAY[1], 
            METALIMNION = METALIMNION[1],
            TEMP_FIELD = mean(TEMP_FIELD),
            DO_FIELD = mean(DO_FIELD),
            PH_FIELD = mean(PH_FIELD),
            COND_FIELD = mean(COND_FIELD)
  )


# Create a layer variable: each observation will be assigned to a layer
# e = epilimnion or no stratification
# m = metalimnion
# h = hypolimnion
nla.2007.profile.4 = nla.2007.profile.3 %>%
  mutate(LAYER = 0)

nla.2007.profile.4$LAYER[1] = "E"

for (i in 2:nrow(nla.2007.profile.4)) {
  # an observation that is at the top or bottom of the metalimnion is in the metalimnion
  if (nla.2007.profile.4$METALIMNION[i] == "T" | 
      nla.2007.profile.4$METALIMNION[i] == "B")
  { nla.2007.profile.4$LAYER[i] = "M" }
  
  # observations below the bottom of the metalimnion is in the hypolimnion
  else if (nla.2007.profile.4$SITE_ID[i-1] == nla.2007.profile.4$SITE_ID[i] &
           nla.2007.profile.4$VISIT_NO[i-1] == nla.2007.profile.4$VISIT_NO[i] &
           nla.2007.profile.4$METALIMNION[i-1] == "B")
  { nla.2007.profile.4$LAYER[i] = "H" }
  
  # observations on the same site and visit are in the same layer as the one above them
  # (if they don't cross the top or bottom of the metalimnion)
  else if(nla.2007.profile.4$SITE_ID[i-1] == nla.2007.profile.4$SITE_ID[i] &
          nla.2007.profile.4$VISIT_NO[i-1] == nla.2007.profile.4$VISIT_NO[i]) 
  { nla.2007.profile.4$LAYER[i] = nla.2007.profile.4$LAYER[i-1] } 
  
  # observations on another site or visit start at the epilimnion
  else  { nla.2007.profile.4$LAYER[i] = "E" }
}



# Remove unuseful variable
nla.2007.profile.5 = nla.2007.profile.4 %>%
  select(-METALIMNION)


# Change variables class
nla.2007.profile.5$SITE_ID = as.factor(nla.2007.profile.5$SITE_ID)
nla.2007.profile.5$VISIT_NO = as.factor(nla.2007.profile.5$VISIT_NO)
nla.2007.profile.5$DEPTH = as.numeric(nla.2007.profile.5$DEPTH)
nla.2007.profile.5$YEAR = as.numeric(nla.2007.profile.5$YEAR)
nla.2007.profile.5$MONTH = as.numeric(nla.2007.profile.5$MONTH)
nla.2007.profile.5$DAY = as.numeric(nla.2007.profile.5$DAY)
nla.2007.profile.5$TEMP_FIELD = as.numeric(nla.2007.profile.5$TEMP_FIELD)
nla.2007.profile.5$DO_FIELD = as.numeric(nla.2007.profile.5$DO_FIELD)
nla.2007.profile.5$PH_FIELD = as.numeric(nla.2007.profile.5$PH_FIELD)
nla.2007.profile.5$COND_FIELD = as.numeric(nla.2007.profile.5$COND_FIELD)
nla.2007.profile.5$LAYER = as.factor(nla.2007.profile.5$LAYER)



# Export processed data frame
write.table(nla.2007.profile.5,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_profile_processed.tsv",
            sep = "\t")





#### 1.2 Tidy the 2012 lake profiles data set ####
nla.2012.profile.2 = nla.2012.profile %>%
  # Only PROF observations are kept (we don't want calibration values)
  # Observations without site_id or depth values are removed 
  filter(SITE_ID != "", SAMPLE_TYPE == "PROF", !is.na(DEPTH)) %>%
  select(-SAMPLE_TYPE) %>%
  # Extract the year, month and day of the sample event
  separate(DATE_PROFILE, into = c("YEAR", "MONTH", "DAY"), sep = "-") %>%
  mutate(DAY = substr(DAY, start = 1, stop = 2)) %>%
  # Sort by depth for each site
  arrange(SITE_ID, VISIT_NO, DEPTH)


nla.2012.profile.3 = nla.2012.profile.2 %>%
  # Average observations on same site, visit no and depth 
  group_by(SITE_ID, VISIT_NO, DEPTH) %>%
  summarise(YEAR = YEAR[1], 
            MONTH = MONTH[1], 
            DAY = DAY[1], 
            METALIMNION = METALIMNION[1],
            TEMP_FIELD = mean(TEMP_FIELD),
            DO_FIELD = mean(DO_FIELD),
            PH_FIELD = mean(PH_FIELD),
            COND_FIELD = mean(COND_FIELD)
            )


# Create a layer variable: each observation will be assigned to a layer
# e = epilimnion or no stratification
# m = metalimnion
# h = hypolimnion
nla.2012.profile.4 = nla.2012.profile.3 %>%
  mutate(LAYER = 0)

nla.2012.profile.4$LAYER[1] = "E"

for (i in 2:nrow(nla.2012.profile.4)) {
  # an observation that is at the top or bottom of the metalimnionit is in the metalimnion
  if (nla.2012.profile.4$METALIMNION[i] == "T" | 
           nla.2012.profile.4$METALIMNION[i] == "B")
  { nla.2012.profile.4$LAYER[i] = "M" }
  
  # observations below the bottom of the metalimnion is in the hypolimnion
  else if (nla.2012.profile.4$SITE_ID[i-1] == nla.2012.profile.4$SITE_ID[i] &
           nla.2012.profile.4$VISIT_NO[i-1] == nla.2012.profile.4$VISIT_NO[i] &
           nla.2012.profile.4$METALIMNION[i-1] == "B")
  { nla.2012.profile.4$LAYER[i] = "H" }
  
  # observations on the same site and visit are in the same layer as the one above them
  # (if they don't cross the top or bottom of the metalimnion)
  else if(nla.2012.profile.4$SITE_ID[i-1] == nla.2012.profile.4$SITE_ID[i] &
     nla.2012.profile.4$VISIT_NO[i-1] == nla.2012.profile.4$VISIT_NO[i]) 
  { nla.2012.profile.4$LAYER[i] = nla.2012.profile.4$LAYER[i-1] } 
  
  # observations on another site or visit start at the epilimnion
  else  { nla.2012.profile.4$LAYER[i] = "E" }
}
  


# Remove unuseful variable
nla.2012.profile.5 = nla.2012.profile.4 %>%
  select(-METALIMNION)

nla.2012.profile.5$SITE_ID = as.factor(nla.2012.profile.5$SITE_ID)
nla.2012.profile.5$VISIT_NO = as.factor(nla.2012.profile.5$VISIT_NO)
nla.2012.profile.5$DEPTH = as.numeric(nla.2012.profile.5$DEPTH)
nla.2012.profile.5$YEAR = as.numeric(nla.2012.profile.5$YEAR)
nla.2012.profile.5$MONTH = as.numeric(nla.2012.profile.5$MONTH)
nla.2012.profile.5$DAY = as.numeric(nla.2012.profile.5$DAY)
nla.2012.profile.5$TEMP_FIELD = as.numeric(nla.2012.profile.5$TEMP_FIELD)
nla.2012.profile.5$DO_FIELD = as.numeric(nla.2012.profile.5$DO_FIELD)
nla.2012.profile.5$PH_FIELD = as.numeric(nla.2012.profile.5$PH_FIELD)
nla.2012.profile.5$COND_FIELD = as.numeric(nla.2012.profile.5$COND_FIELD)
nla.2012.profile.5$LAYER = as.factor(nla.2012.profile.5$LAYER)

# Export processed data frame
write.table(nla.2012.profile.5,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2012_profile_processed.tsv",
            sep = "\t")






#### 2.1 Tidy the 2007 site infos data set ####


# Import the interim data sets 
nla.2007.infos = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/sites_infos/nla2007_sitesinfos.tsv", header = TRUE,  sep = '\t')
nla.2012.infos = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/sites_infos/nla2012_sitesinfos.tsv", header = TRUE,  sep = '\t', quote = "\\")


# Select useful variables
nla.2007.infos.2 = nla.2007.infos %>%
  select(SITE_ID, VISIT_NO, DAY, MONTH, YEAR, LAT_DD, LON_DD,
         ST, EPA_REG, WSA_ECO9, HUC_2, HUC_8, LAKE_ORIGIN, 
         LAKEAREA, LAKEPERIM, ELEV_PT, DEPTHMAX, SLD,
         WGT_NLA)

# Rename columns 
colnames(nla.2007.infos.2) = c("SITEID_07", "VISIT_NO", "DAY", "MONTH", "YEAR", "LAT", "LON",
                               "STATE", "EPA_REG", "ECO9", "HUC2", "HUC8", "LAKE_ORIGIN", 
                               "AREA_KM2", "PERIM_KM", "ELEVATION_M", "DEPTHMAX_M", "SLD",
                               "WGT")

# Change the variables' class
nla.2007.infos.3  = nla.2007.infos.2 %>%
  mutate(SITEID_07 = as.factor(SITEID_07),
         VISIT_NO = as.numeric(VISIT_NO), 
         DAY = as.numeric(DAY), MONTH = as.numeric(MONTH), YEAR = as.numeric(YEAR),
         LAT = as.numeric(LAT), LON = as.numeric(LON), 
         STATE = as.factor(STATE),
         EPA_REG = as.factor(EPA_REG), ECO9 = as.factor(ECO9),
         HUC2 = as.factor(HUC2), HUC8 = as.factor(HUC8),
         LAKE_ORIGIN = as.factor(LAKE_ORIGIN), 
         AREA_KM2 = as.numeric(AREA_KM2), PERIM_KM = as.numeric(PERIM_KM), 
         ELEVATION_M = as.numeric(ELEVATION_M), 
         DEPTHMAX_M = as.numeric(DEPTHMAX_M), SLD = as.numeric(SLD), 
         WGT = as.numeric(WGT))


# Export the processed data set
write.table(nla.2007.infos.3,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/sites_infos/nla2007_infos.tsv",
            sep = "\t")



#### 2.2 Tidy the 2012 site infos data set ####

# Select useful variables
# The 2012 data set does not have information on maximum depth nor on shoreline development (SLD)
nla.2012.infos.2 = nla.2012.infos %>%
  select(SITE_ID, SITEID_07, SITESAMP, VISIT_NO, DAY, MONTH, YEAR, LAT_DD83, LON_DD83,
         STATE, EPA_REG, FW_ECO9, HUC2, HUC8, LAKE_ORIGIN, 
         AREA_HA, PERIM_KM, ELEVATION, 
         WGT_ALL) 

# Rename columns according to the ones of the 2007 data set
colnames(nla.2012.infos.2) = c("SITEID_12", "SITEID_07", "SITESAMP", "VISIT_NO", "DAY", "MONTH", "YEAR", "LAT", "LON",
                               "STATE", "EPA_REG", "ECO9", "HUC2", "HUC8", "LAKE_ORIGIN", 
                               "AREA_KM2", "PERIM_KM", "ELEVATION_M",
                               "WGT")

# Change the variables' class
# Filter for sampled sites only
nla.2012.infos.3  = nla.2012.infos.2 %>%
  mutate(SITEID_12 = as.factor(SITEID_12), SITEID_07 = as.factor(SITEID_07), 
         SITESAMP = as.factor(SITESAMP), VISIT_NO = as.numeric(VISIT_NO), 
         DAY = as.numeric(DAY), MONTH = as.numeric(MONTH), YEAR = as.numeric(YEAR),
         LAT = as.numeric(LAT), LON = as.numeric(LON), 
         STATE = as.factor(STATE),
         EPA_REG = as.factor(EPA_REG), ECO9 = as.factor(ECO9),
         HUC2 = as.factor(HUC2), HUC8 = as.factor(HUC8),
         LAKE_ORIGIN = as.factor(LAKE_ORIGIN), 
         AREA_KM2 = as.numeric(AREA_KM2) / 100, PERIM_KM = as.numeric(PERIM_KM), 
         ELEVATION_M = as.numeric(ELEVATION_M), 
         WGT = as.numeric(WGT)) %>% 
  filter(SITESAMP == "Y")


# Export the processed data set
write.table(nla.2012.infos.3,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/sites_infos/nla2012_infos.tsv",
            sep = "\t")



#### 3.1 Repeated sites infos ####


# Data table to link sites from 2007 to those of 2012
common.sites = inner_join(nla.2007.infos.3, nla.2012.infos.3, by = "SITEID_07") %>%
  select(SITEID_07, SITEID_12) %>%
  distinct(SITEID_07, SITEID_12, .keep_all = TRUE)
dim(common.sites)


# Join the infos data set and only keep the sites repeated in 2007 and 2012
nla.2007.2012.infos = inner_join(nla.2007.infos.3, nla.2012.infos.3, by = "SITEID_07")

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


#### 3.2 Complete site infos data set ####

# Append data sets as new rows
nla.2007.2012.infos.all = bind_rows(nla.2007.2012.infos.repeated, nla.2007.infos.3) %>%
  bind_rows(nla.2012.infos.3)

# All NA years are in 2012
# Change those NAs to 2012
which.NA.2012 = which(is.na(nla.2007.2012.infos.all$YEAR)) 
nla.2007.2012.infos.all[which.NA.2012, "YEAR"] = 2012

# Assign adequate ID for sites
for (i in 1:nrow(nla.2007.2012.infos.all)) {
  if (is.na(nla.2007.2012.infos.all$SITE_ID[i])) {
    if (nla.2007.2012.infos.all$YEAR[i] == 2007) {
      nla.2007.2012.infos.all$SITE_ID[i] = as.character(nla.2007.2012.infos.all$SITEID_07[i])
    }
    if (nla.2007.2012.infos.all$YEAR[i] == 2012) {
      nla.2007.2012.infos.all$SITE_ID[i] = as.character(nla.2007.2012.infos.all$SITEID_12[i])
    }
  }
}


# Correctly order observations and remove identical rows
nla.2007.2012.infos.all = nla.2007.2012.infos.all %>%
  select(-SITEID_07, -SITEID_12, -SITESAMP) %>%
  arrange(SITE_ID, YEAR, VISIT_NO) %>%
  distinct()


# Export the processed data set
write.table(nla.2007.2012.infos.all,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/sites_infos/nla2007_2012_infos_all.tsv",
            sep = "\t")



#### 3.3 Complete lake profiles data set ####

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


#### 3.4 Repeated lake profiles ####

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



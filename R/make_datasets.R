### Francis Banville - Université de Montréal
### May 14th 2019

### The interim 2007 and 2012 lake profiles data sets, obtained from Open Refine, are here filtered and merged to form a final processed data set

# Libraries
library("dplyr")
library("ggplot2")
library("tidyr")

# Import the data sets 
# The data sets are in the folder "interim"
nla.2007.profile = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/lakes_profile/nla2007_profile_20091008.tsv", header = TRUE,  sep = '\t')
nla.2012.profile = read.table("C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/interim/lakes_profile/nla2012_wide_profile_08232016.tsv", header = TRUE,  sep = '\t')


## 1. Tidy the 2007 lake profiles data set
nla.2007.profile.2 = nla.2007.profile %>%
  # Only PROF observations are kept (we don't want calibration values)
  # Observations without site_id or depth values are removed 
  filter(SITE_ID != "", !is.na(DEPTH)) %>%
  # Unuseful variables are also removed
  select(-YEAR, -SAMPLED_PROFILE, -FLAG_PROFILE, -COMMENT_PROFILE) %>%
  # Extract the year, month and day of the sample event
  separate(DATE_PROFILE, into = c("YEAR", "MONTH", "DAY"), sep = "-") %>%
  mutate(DAY = substr(DAY, start = 1, stop = 2)) %>%
  # Sort by depth for each site
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
  # an observation that is at the top or bottom of the metalimnionit is in the metalimnion
  if (nla.2007.profile.4$METALIMNION[i] == "T" | 
      nla.2007.profile.4$METALIMNION[i] == "B")
  { nla.2007.profile.4$LAYER[i] = "M" }
  
  # observations below the bottom of the metalimnion is in the hypolimnion
  else if (nla.2007.profile.4$SITE_ID[i-1] == nla.2007.profile.4$SITE_ID[i] &
           nla.2007.profile.4$VISIT_NO[i-1] == nla.2007.profile.4$VISIT_NO[i-1] &
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


# View(tail(nla.2007.profile.4,200))

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






## 2. Tidy the 2012 lake profiles data set
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
           nla.2012.profile.4$VISIT_NO[i-1] == nla.2012.profile.4$VISIT_NO[i-1] &
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
  

# View(tail(nla.2012.profile.4,200))

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




## 3. Merge data sets
nla.2007.2012.profile = union(nla.2007.profile.5, nla.2012.profile.5)

# Reorder obervations
nla.2007.2012.profile.2 = nla.2007.2012.profile %>%
  arrange(YEAR, SITE_ID, VISIT_NO, DEPTH)

# Assign adequate class to SITE_ID
nla.2007.2012.profile.2$SITE_ID = as.factor(nla.2007.2012.profile.2$SITE_ID)

# Export merged data frame
write.table(nla.2007.2012.profile.2,
            file = "C:/Users/Francis Banville/Documents/Biologie_quantitative_et_computationnelle/Travaux_dirigés/Travail_dirige_II/US_LakeProfiles/data/processed/lakes_profile/nla2007_2012_merged.tsv",
            sep = "\t")





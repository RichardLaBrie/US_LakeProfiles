## Tidying of the 2007 and 2012 site infos data sets using R 
## See file "make_datasets.R"

We made 4 processed data sets for site infos (metadata):
- nla2007_infos.tsv
- nla2012_infos.tsv
- nla2007_2012_infos_repeated.tsv
- nla2007_2012_infos_all.tsv


## nla2007_infos.tsv ##

Tidy data set of site infos in 2007. We cleaned the interim data set "nla2007_sitesinfos" previously obtained with Open Refine. We changed the class of selected and renamed variables. 

Our processed data set contained those variables:
_SITEID 07_: ID assigned to each site in 2007
_VISIT NO_: sequential visit number within year
_DAY_: day of sample collection
_MONTH_: month of sample collection
_YEAR_: year of sample collection (always 2007)
_LAT_: latitude (decimal degrees) obtained from NHD (NAD83)
_LON_: longitude (decimal degrees) obtained from NHD (NAD83)
_STATE_: state two letter code
_EPA REG_: EPA region
_ECO9_: Wadeable Stream Assessment nine aggregrated Omernik level 3 ecoregions
_HUC2_: Hydrologic region (2-digit)
_HUC8_: Hydrologic region (8-digit)
_LAKE ORIGIN_: lake origin (MAN-MADE, NATURAL )which includes natural lakes augmented by dams))
_AREA KM2_: Lake polygon area (km^2) from NHD 
_PERIM KM_: Lake polygon perimeter (km) from NHD
_ELEVATION M_: Site elevation (m) from the National Elevation Dataset
_DEPTHMAX M_: Maximum Observed Lake Depth (m)
_SLD_: Shoreline development index (=LAKEPERIM/(2sqrt(LAKEAREA * pi))
_WGT_: Adjusted site weight. To be used for population estimation




## nla2012_infos.tsv ##

Tidy data set of site infos in 2007. We cleaned the interim data set "nla2007_sitesinfos" previously obtained with Open Refine. We changed the class of selected and renamed variables. We filtered for sampled sites only (SITESAMP = Y).

Our processed data set contained those variables:
_SITEID 12_: Identification code for site assigned in 2012
_SITEID 07_: Site ID assigned in 2007 NLA
_SITESAMP_: Did you sample this site (Y/N)
_VISIT NO_: Sequential number of visit to site
_DAY_: day of sample collection
_MONTH_: month of sample collection
_YEAR_: year of sample collection (always 2007)
_LAT_: latitude (based on NAD83 datum) assigned to lake during site selection from NHD-based sample frame. Generally, but not always, represents centroid of lake polygon in NHD
_LON_: longitude (based on NAD83 datum) assigned to lake during site selection from NHD-based sample frame. Generally, but not always represents centroid of lake polygon in NHD
_STATE_: State lake is assigned to for sampling in NLA 2012	
_EPA REG_: EPA region
_ECO9_: NARS 9-level reporting region (2015), based on aggregated Omernik Level III ecoregions
_HUC2_: USGS Level 2 Hydrologic Unit Code where lake is located			
_HUC8_: USGS Level 8 Hydrologic Unit Code where lake is located		
_LAKE ORIGIN_: Lake origin based on codes used in NLA 2007
_AREA KM2_: Surface area of lake based on NHD polygon (km2)
_PERIM KM_: NHD lake polygon perimeter (km)
_ELEVATION M_: Elevation (m) at lake coordinates (LAT, LON) from NHD Digital Elevation Map layer
_WGT_: Adjusted weight for site. To be used for NLA national population estimation	

Sites of 2012 don't have information on maximum depth or shoreline development index. 


## nla2007_2012_infos_repeated.tsv ##

Tidy data set of repeated site infos in 2007 and 2012. We first identified the sites that were sampled in both 2007 and 2012 using the variable SITEID_07. We found 401 sites sampled in both years. We joined the two preceding data sets concerving rows that appeared in either or both of them (bind_rows), while only keeping the sites sampeld in both years. We changed the ID of the 2012 sites that had also been sampled in 2007 into the ID it first had in 2007. We correctly assigned the year of observations when it was missing (when the year was missing, it happened to always be in 2012). 

The resulting processed data set has every variable listed above, except SITESAMP. The variables SITEID_07 and SITEID_12 were replaced with SITE_ID.

_SITE ID_: Site ID assigned in 2007 NLA


## nla2007_2012_infos_all.tsv ##

Tidy data set of all site infos in 2007 and 2012. We combined the data set of the sites sampled in both 2007 and 2012 to those of the sites sampled in 2007 and of the sites sampled in 2012 (appended as new rows). We changed the ID of the 2012 sites that had also been sampled in 2007 into the ID it first had in 2007. We correctly assigned the year of observations when it was missing (when the year was missing, it happened to always be in 2012). 

The resulting processed data set has the same variables as the ones of the nla2007_2012_infos_repeated data set. 

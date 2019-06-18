## Tidying of the 2007 and 2012 lake profiles data sets using R 
## See file "make_datasets.R"

We made 4 processed data sets for lake profiles:
- nla2007_profile_processed.tsv
- nla2012_profile_processed.tsv
- nla2007_2012_profile_all.tsv
- nla2007_2012_profile_repeated.tsv


## nla2007_profile_processed.tsv ##

Tidy data set of profiles sampled in 2007. We used the interim data set "nla2007_profile" previously obtained with Open Refine. We began by removing observations with unknown site id or depth. When several observations were taken at the same site, on the same visit and at the same depth, we took the mean of their metrics. We also created a variable "layer", that specified the layer in which the observations, at their specified depths, were taken (E = epilimnion or non statified, M = metalimnion and H = hypolimnion). This variable is based on NLA layer identification. We did not consider the data flags or comments when they were present. 

Our processed data set contained those variables:
_SITE ID_: lake site identification code in 2007
_VISIT NO_: whithin-year sample visit number
_DEPTH_: profile depth (m)
_YEAR_: sample year (always 2007)
_MONTH_: sample month
_DAY_: sample day
_TEMP FIELD_: field water temperature (oC)
_DO FIELD_: field dissolved oxygen (mg/L)
_PH FIELD_: field PH
_COND FIELD_: field conductivity (uS at 25 oC assuming 2.2 %/oC)
_LAYER_: sample layer according to the NLA (E = epilimnion or non stratified, M = metalimnion, H = hypolimnion)


## nla2012_profile_processed.tsv ##

Tidy data set of profiles sampled in 2012. We used the interim data set
"nla2012_profile" previously obtained with Open Refine. The processed data set was obtained in a similar manner as its 2007 conterpart. We however only kept profile values (SAMPLE_TYPE = PROF), and did not consider calibration observations (SAMPLE_TYPE = CALIB). The sample type was not specified in the 2007 data set. No flags or comments were specified, in contrary of the 2007 interim data set. 

Our processed data set contained those variables:
_SITE ID_: Identification code for site in 2012
_VISIT NO_: Sequential number of visit to site
_DEPTH_: depth for profile measurements (m)
_YEAR_: sample year (always 2012)
_MONTH_: sample month
_DAY_: sample day
_TEMP FIELD_: termperature for index profile (oC)
_DO FIELD_: oxygen for index profile (mg/L)
_PH FIELD_: PH for index profile
_COND FIELD_: conductivity for index profile (uS/cm)
_LAYER_: sample layer according to the NLA (E = epilimnion or non stratified, M = metalimnion, H = hypolimnion)


## nla2007_2012_profile_all.tsv ##

We joined the two preceding data sets concerving rows that appeared in either or both of them (union). We changed the ID of the 2012 sites that had also been sampled in 2007 into the ID it first had in 2007. We identified those sites using the SITEID_07 variable of the 2012 site infos data set, which represent the ID the site had in 2007, when it was also sampled in 2007. This data set thus contains sites that were only sampled in 2007, sites that were only sampled in 2012 and sites that were sampled both years. 

The variables of this data set are the same as the ones listed above. 


## nla2007_2012_profile_repeated.tsv ##

We only kept the sites that were sampled in both 2007 and 2012 in this data set. We decided to identify those sites with their ID in 2007 only. 

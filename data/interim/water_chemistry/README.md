## Tidying of the 2007 and 2012 lake profiles data sets using OpenRefine

Operation histories were extracted in JSON format and were added in the folder named "OpenRefine".

We kept every variable that were present in both data sets:
- **SITE_ID**: Lake Site Identification Code	
- **YEAR** AND/OR **DATE_PROFILE** (named DATE_COL in raw 2012 data set): Date of lake profile
- **VISIT_NO**:	Within-year Sample Visit Number	
- **DEPTH**:	Profile Depth (m)		
- **METALIMNION**:	Field Crew Flag for Depth of Top (T) and Bottom (B) of Metalimnion
- **TEMP_FIELD** (named TEMPERATURE in raw 2012 data set):	Field Water Temperature (oC)	
- **DO_FIELD** (named OXYGEN in raw 2012 data set):	Field Dissolved Oxygen (mg/L)	
- **PH_FIELD** (named PH in raw 2012 data set):	Field pH		
- **COND_FIELD** (named CONDUCTIVITY in raw 2012 data set):	Field Conductivity (uS at 25 oC assuming 2.2%/oC)	

It is worth noting that the former variable "OXYGEN" of the 2012 data set was considered as dissolved oxygen only so that it corresponds to the "DO-FIELD" variable of the 2007 data set. Also, the former "CONDUCTIVITY" variable of the 2012 data set was expressed in uS/cm, whereas the "DO_FIELD" variable of the 2007 data set was expressed in uS (at 25°C assuming 2.2%/°C). We don't know if conductivity was measured similarly in 2007 and 2012. 

Also, other variables were kept for future filtering purposes:
In the 2007 data set:
- **SAMPLED_PROFILE**: SAMPLED variable from profile data
- **FLAG_PROFILE**	Data flag from lake profile form	
- **COMMENT_PROFILE**	Comment and flag explanation

In the 2012 data set:
**SAMPLE_TYPE**: Sample type

The remaining variables were excluded because they referred to calibration only or were not comparable between the two data sets. 
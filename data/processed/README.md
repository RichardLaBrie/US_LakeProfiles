# Processed data sets


## info_0712 
Observations: sampling events (i.e a site sampled in a specific year and visit number)

Variables: 
- siteid_07: site identification number in 2007
- sampling_event: sampling event identification number, composed of the site id, the year and the visit number
- visit_no: within-year visit number
- day: day of sampling event
- month: month of sampling event              
- year: year of sampling event              
- lat: latitude (in decimal degrees)                
- lon: longitude (in decimal degrees)     
- state: US state abbreviation
- EPA_reg: EPA region           
- ECO9: NARS 9-level reporting region, based on aggregated Omernik Level III ecoregions: CPL=Coastal Plains; NAP=Northern Appalachians; NPL=Northern Plains; SAP=Southern Appalachians; SPL=Southern Plains; TPL=Temperate Plains; UMW=Upper Midwest; WMT=Western Mountains; XER=Xeric West.  
- HUC2: Hydrologic region (2-digit)
- HUC8: Hydrologic region (8-digit)              
- lake_origin: natural or man-made        
- area_km2: area of the lake (km2)         
- perim_km: perimeter of the lake (km)           
- elevation_m: elevation of the lake (m)       
- depthmax_m: maximum depth (m)       
- SLD: shoreline development index (= LAKEPERIM/(2 sqrt(LAKEAREA x pi))    
- WGT: Adjusted site weight. USE for population estimation
- secchi_m: Secchi depth (m)           
- clear_to_bottom: wether the water is clear to bottom or not    
- basinarea_km2: basin area (km2)     
- pct_forest: % of forest in the basin area        
- pct_agric: % of agriculture in the basin area          
- chla_ugL: concentration of chlorophyll-a (ug/L)           
- color_PCU: water color (PCU)          
- NTL_ugL: concentration of total nitrogen (ug/L)            
- PTL_ugL: concentration of total phosphorus (ug/L)            
- NA_mgL: concentration of sodium (mg/L)           
- DOC_mgL: concentration of dissolved organic matter (DOC) (mg/L)            
- cond_uScm: conductivity (uS/cm)          
- turb_NTU: turbidity (NTU)           
- nutrient_color: nutrient-color status     
- siteid_12: site identification number in 2012          
- UID: unique site identification number                
- WALA_ratio: ratio of basin area : lake area        
- site_id: site id in 2012 if the lake was only sampled in 2012, site id in 2007 if not           
- sampled_depthmax_m: maximum sampled depth (m) 
- resampled: wether the site was sampled in 2007 and 2012 (1) or only one of those years (0)          
- Julian.day: Julian day of sampling event       
- precip_5: monthly state total precipitation averaged on 150 days before sampling event          
- avgtemp_5: monthly state average temperature averaged on 150 days before sampling event         
- mintemp_5: monthly state average minimum temperature averaged on 150 days before sampling event           
- maxtemp_5: monthly state average maximum temperature averaged on 150 days before sampling event       
- precip_4: monthly state total precipitation averaged on 120 days before sampling event             
- avgtemp_4: monthly state average minimum temperature averaged on 120 days before sampling event   
- mintemp_4: monthly state average minimum temperature averaged on 120 days before sampling event       
- maxtemp_4: monthly state average maximum temperature averaged on 120 days before sampling event
- precip_3: monthly state total precipitation averaged on 90 days before sampling event           
- avgtemp_3: monthly state average minimum temperature averaged on 90 days before sampling event           
- mintemp_3: monthly state average minimum temperature averaged on 90 days before sampling event        
- maxtemp_3: monthly state average maximum temperature averaged on 90 days before sampling event          
- precip_2: monthly state total precipitation averaged on 60 days before sampling event          
- avgtemp_2: monthly state average minimum temperature averaged on 60 days before sampling event   
- mintemp_2: monthly state average minimum temperature averaged on 60 days before sampling event        
- maxtemp_2: monthly state average maximum temperature averaged on 60 days before sampling event          
- precip_1: monthly state total precipitation averaged on 30 days before sampling event           
- avgtemp_1: monthly state average minimum temperature averaged on 30 days before sampling event          
- mintemp_1: monthly state average minimum temperature averaged on 30 days before sampling event       
- maxtemp_1: monthly state average maximum temperature averaged on 30 days before sampling event      



## profile_0712
Observations: profiles of sampling events (specific depth at a specific site sampled in a specific year and visit number)

Variables: 
- sampling_event: sampling event identification number, composed of the site id, the year and the visit number
- site_id: site id in 2012 if the lake was only sampled in 2012, site id in 2007 if not      
- visit_no: within-year visit number      
- depth: depth of observation           
- year: year of sampling event                          
- month: month of sampling event                       
- day: day of sampling event                      
- temp: temperature at the depth (0C)           
- DO: concentration of dissolved oxygen at the depth (mg/L)              
- PH: PH at the depth             
- cond: conductivity at the depth (uS/cm)          
- layer_nla: layer (epilimnion, metalimnion or hypolimnnion) at the depth according to the NLA
- resampled: wether the site was sampled in 2007 and 2012 (1) or only one of those years (0)       
 
 
## Strat_0712
Observations: sampling events (i.e a site sampled in a specific year in its first visit only)

Variables: 
- site_id: site id in 2012 if the lake was only sampled in 2012, site id in 2007 if not                 
- resampled: wether the site was sampled in 2007 and 2012 (1) or only one of those years (0)          
- type: 1 = epi/meta/hypo, 2 = epi/meta, 3 = meta/hypo, 4 = epi/hypo, 5 = epi, 6 = meta              
- type_simple: 1 = epi/meta/hypo, 2 = epi/meta, 3 = epi, 4 = other     
- stratified: wether the lake was stratified or not 
- deltaT: difference of temperature between top and bottom layers of startified lakes (0C)        
- epithick: epilimnetic thickness (m)          
- thermodepth: depth of the thermocline (m)      
- anoxiaV: anoxia volume (m3)           
- hypoxiaV: hypoxia volume (m3)           
- schmidth_stability: Schmidt stability index (J/m2)
- month: month of sampling event   
- year: year of sampling event               
- Julian_day: Julian day of sampling event        
- lat: latitude (in decimal degrees)               
- lon: longitude (in decimal degrees)            
- elevation: elevation of the lake (m)        
- ECO9: NARS 9-level reporting region, based on aggregated Omernik Level III ecoregions: CPL=Coastal Plains; NAP=Northern Appalachians; NPL=Northern Plains; SAP=Southern Appalachians; SPL=Southern Plains; TPL=Temperate Plains; UMW=Upper Midwest; WMT=Western Mountains; XER=Xeric West.          
- lake_origin: natural or man-made       
- area: area of the lake (km2)             
- volume: volume of the lake (m3)          
- WALA_ratio: ratio of basin area : lake area        
- depth: maximum depth (m)              
- SDI: shoreline development index (= LAKEPERIM/(2 sqrt(LAKEAREA x pi))               
- forest: % of forest in the basin area            
- agric:  % of agriculture in the basin area              
- precip_5: monthly state total precipitation averaged on 150 days before sampling event          
- avgtemp_5: monthly state average temperature averaged on 150 days before sampling event         
- mintemp_5: monthly state average minimum temperature averaged on 150 days before sampling event           
- maxtemp_5: monthly state average maximum temperature averaged on 150 days before sampling event       
- precip_4: monthly state total precipitation averaged on 120 days before sampling event             
- avgtemp_4: monthly state average minimum temperature averaged on 120 days before sampling event   
- mintemp_4: monthly state average minimum temperature averaged on 120 days before sampling event       
- maxtemp_4: monthly state average maximum temperature averaged on 120 days before sampling event
- precip_3: monthly state total precipitation averaged on 90 days before sampling event           
- avgtemp_3: monthly state average minimum temperature averaged on 90 days before sampling event           
- mintemp_3: monthly state average minimum temperature averaged on 90 days before sampling event        
- maxtemp_3: monthly state average maximum temperature averaged on 90 days before sampling event          
- precip_2: monthly state total precipitation averaged on 60 days before sampling event          
- avgtemp_2: monthly state average minimum temperature averaged on 60 days before sampling event   
- mintemp_2: monthly state average minimum temperature averaged on 60 days before sampling event        
- maxtemp_2: monthly state average maximum temperature averaged on 60 days before sampling event          
- precip_1: monthly state total precipitation averaged on 30 days before sampling event           
- avgtemp_1: monthly state average minimum temperature averaged on 30 days before sampling event          
- mintemp_1: monthly state average minimum temperature averaged on 30 days before sampling event       
- maxtemp_1: monthly state average maximum temperature averaged on 30 days before sampling event            
- chla:  concentration of chlorophyll-a (ug/L)               
- color: water color (PCU)             
- TN: concentration of total nitrogen (ug/L)              
- TP: concentration of total phosphorus (ug/L)         
- DOC: concentration of dissolved organic matter (DOC) (mg/L)               
- cond: conductivity (uS/cm)            
- turb: turbidity (NTU)            
- nutrient_color: nutrient-color status 
# Data folders

The three data folders contain raw, interim and processed data sets. 

Raw data sets were downloaded as is from the [National Aquatic Resource Surveys (NARS) database](https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys). They were put in subfolders according to the NARS indicators. The CSV files contain the data, while the TXT files contain their metadata (i.e. variable descriptions). Some data sets relate to the 2007 lake survey, while others relate to the 2012 lake survey. The file names were unchanged from that of the NARS, and contain the surveys' year at their beginning. The subfolder "climate_noaa" contains data from the [National Centers for Environmental Information (NOAA)](https://www.ncdc.noaa.gov/cag/national/time-series/110/pcp/ytd/12/1895-2016?base_prd=true&firstbaseyear=1901&lastbaseyear=2000) about state wide monthly averaged temperature, monthly minimum temperature, monlthyl maximum temperature and monthly total precipitation. 



Interim data sets were obtained after cleaning the raw data sets with OpenRefine. They were also put in subfolders according to the NARS indicators. The TSV files contain the interim data sets, while the JSON files contain OpenRefine operation histories. 



Processed data sets were obtained after cleaning the interim data sets using R. The code used to tidy and merge the interim data sets is in *make_datasets.R*. The data set info_0712.tsv represent metrics of sampling events (i.e. sites sampled in a specific year in a specific visit). The data set profile_0712.tsv represent metrics of profiles of sampling events (i.e. metrics taken at some specific depths of a site). Finally, the data set strat_0712.tsv represent metrics of sampling events, including those computed from rLakeAnalyzer, that are used in the analysis.
  




The file *nla2007_finaldatanotes* contains supplementary information about data collection. 






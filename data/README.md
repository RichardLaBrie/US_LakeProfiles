# Data folders

The three data folders contain raw, interim and processed data sets. 

Raw data sets were downloaded as is from the [National Aquatic Resource Surveys (NARS) database](https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys). They were put in subfolders according to the NARS indicators. The CSV files contain the data, while the TXT files contain their metadata (i.e. variable descriptions). Some data sets relate to the 2007 lake survey, while others relate to the 2012 lake survey. The file names were unchanged from that of the NARS, and contain the surveys' year at their beginning.  

Interim data sets were obtained after cleaning the raw data sets with OpenRefine. They were also put in subfolders according to the NARS indicators. The TSV files contain the interim data sets, while the JSON files contain OpenRefine operation histories. 

Processed data sets were obtained after cleaning the interim data sets using R. The code used to tidy and merge the interim data sets is in *make_datasets.R*.


### Prerequisites

OpenRefine 3.1 and R 3.6.0. 


## Authors

* **Francis Banville** - *Initial work* - [FrancisBanville](https://github.com/FrancisBanville)




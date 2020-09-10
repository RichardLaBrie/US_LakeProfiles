# R SCRIPT

Our code is divided into 4 scripts:

1. make_datasets.R
The interim data sets obtained from OpenRefine are processed and merged to make final data sets that will be used in the analysis and visualization. We here used the package rLakeAnalyzer to compute some lake metrics. For example, bathymetric curves, volumetrically averaged lake temperature and lake stratification were computed. Three processed data sets were produced: 
    
   A. info_0712.tsv 
   Metrics of sampling events (i.e. sites sampled in a specific year in a specific visit).
    
   B. profile_0712.tsv
   Metrics of profiles of sampling events (i.e. metrics taken at some specific depths of a site).
  
   C. strat_0712.tsv
   Metrics of sampling events, including those computed from rLakeAnalyzer, that will be used in the analysis.
  

2. visualization.R
The vast majority of figures are generated with this script. It is recommanded to run this script before the analysis ones, because of conflicts between packages. 

3. models_and_analysis.R
The data analysis, including some multivariate quantitative analysis, are computed here. This scipt contains the majority of the final results of Francis' directed works. Some figures are also generated here, when they were mostly computed in the context of a broader analysis. 

4. quanti_analysis.R
The code created in the context of the class "Analyse quantitative des donnees biologiques", at Universite de Montreal, is found in this script. For more details on this class project, contact Mr. Francis Banville. 



Finally, the folder "functions" contains some functions created by Drs Daniel Borcard and Pierre Legendre. They have to be in the working directory when called in the scripts. 

## Tidying of the 2007 and 2012 sites infos data sets using OpenRefine

Operation histories were extracted in JSON format and were added in the folder named "OpenRefine".

We removed every variable that were considered redundant or that did not have any values. Some variables that were kept might not be used for further analysis. 

We also clustered some character strings that refered to the same object. For example, we considered the _Surprise Lake_ as identical to the _Lake Surprise_. Some lake names differed by one or two letters only. At this point of the data cleaning process, we did not take any decision in regard of which lake names reffered to the same entities. Indeed, we might not directly use this information in the project, but might instead use QGIS to plot sites coordinates. 

Some other inconsistencies (ex. upper vs lowercase letters) within and between the data sets might be dealt with later if necessary. 
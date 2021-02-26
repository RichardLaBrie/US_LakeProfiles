library(rgdal)
library(rgeos)
library(sp)
library(lakemorpho)

### NOTES
# The input shapefile can contain multiple lakes. The shapefile must be 
# projected, ideally in an equidistant projection relevant to the lakes extent.
# The shapefile must contain at least two fields. The first should be a unique 
# lake ID used to build the output table. The second should be a field 
# containing each lake's perimeter in meters.

### USER PARAMETERS
##############################################################################

lakePath = "B:/GIS_Working/Richard/Fetch_Lake/NLA_2012_Lakes.shp"          ### Path to lakes shapfile
outputCSV = "B:/GIS_Working/Richard/Fetch_Lake/LakeLengths_NLA2012.csv"    ### Path to output CSV file
pointDistance = 50                                                         ### Spacing between evaluated points along the shoreline (in meters)
idFieldName = "NLA12_ID"                                                   ### Field name containing lake IDs
perimeterFieldName = "Perimeter"                                           ### Field name containing lake perimeters (in meters)

##############################################################################

inLakes = readOGR(lakePath)                                                ### Reads the lakes shapefile in SpatialPolygons

finalDF = data.frame(LakeID = character(),                                 ### Creates an empty dataframe
                     Perimeter = double(),
                     NoPoints = integer(),
                     MaxLength = double())

for (i in 1:length(inLakes)) {
  
  inLake = inLakes[i,]                                                     ### Extracts one lake for processing
  inLakeMorpho = lakeMorphoClass(inLake, inElev = NULL, inCatch = NULL,    ### Creates a LakeMorphoClass from the above lake
                                 inLakeDist = NULL)
  inLakeDF = as.data.frame(inLake)                                         ### Creates a dataframe from the lake SpatialPolygon
  perimeter = inLakeDF[1, perimeterFieldName]                              ### Extracts the lake's perimeter (in meters)
  lakeID = inLakeDF[1, idFieldName]                                        ### Extracts the lake's ID
  pointDens = round(perimeter/pointDistance)                               ### Calculates the number of points to be placed evenly along the lake's shoreline
  
  maxLength = lakeMaxLength(inLakeMorpho, pointDens, addLine = FALSE)      ### Calculate the lake's longest open water distance
  
  newRow = data.frame(lakeID = lakeID,                                     ### a new dataframe with the lake's relevant info
                      Perimeter = perimeter,
                      NoPoints = pointDens,
                      MaxLength = maxLength)
  
  finalDF = rbind(finalDF, newRow)                                         ### Appends the new line to the final dataframe
  write.csv(finalDF, outputCSV)                                            ### Writes and overwrites the result in a CSV file
  
  processed = round((i/length(inLakes))*100, digits = 2)                   ### Calculates the percentage of lakes that have been proccessed.
  
  print(paste0("LakeID ", lakeID, " has been processed with ", 
               pointDens, " along its shore."))
  print(paste0("Lake ", i, " of ", length(inLakes), " --- ", 
               processed, "%"))
  print("-------------------------------------")
}
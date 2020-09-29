#Hypolimnion contains the sample name in colnames
#Area is the surface area of the lakes in m2
VolTemp <- function(Hypolimnion, Year, Area)
{
  output = vector(length = dim(Hypolimnion)[2])
  names(output) = colnames(Hypolimnion)
  for(i in 1:length(output))
  {
    data = read.csv(paste0("../data/Preprocessing/",Year,"/",colnames(Hypolimnion)[i], ".csv"), row.names = 1)
    data = data[which(data$depth >= Hypolimnion[3,i]),]
    if(any(is.na(data$temp))) data[is.na(data$temp),"temp"] = min(data$temp,na.rm=T)
    
    Depth = data$depth
    Temperature = data$temp
    max.depth = max(Depth)
    
    output.temp = vector(length = length(Depth)-1)
    Lake.radius = sqrt(Area[i]/pi)
    
    #Calculate for all truncated cone
    for(j in 1:(length(Depth)-2))
    {
      R = (max.depth - Depth[j]) * Lake.radius / max.depth
      r = (max.depth - Depth[j+1]) * Lake.radius / max.depth
      h = Depth[j+1] - Depth[j]
      
      Vol = pi/3 * h * (r*r + r*R + R*R)
      output.temp[j] = Vol * Temperature[j]		
    }
    
    #For last strata, we have a full cone
    Vol = pi/3 * (max.depth - Depth[j+1]) * r * r
    output.temp[j+1] = Vol * Temperature[j+1]
    
    R = (max.depth - Depth[1]) * Lake.radius / max.depth
    Vol = pi/3 * (max.depth - Depth[1]) * R * R
    output[i] = sum(output.temp)/ Vol
  }
  
  return(output)
}

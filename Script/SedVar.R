SedArea <- function(Hypolimnion, Year, Area, Hypo = T)
{
  if(Hypo){
  output = matrix(nrow = dim(Hypolimnion)[2], ncol=3)
  rownames(output) = colnames(Hypolimnion)
  colnames(output) = c("SedArea", "VoltoSedArea", "HypoThick")
  
  for(i in 1:length(Area))
  {
    data = read.csv(paste0("../data/Preprocessing/",Year,"/",colnames(Hypolimnion)[i], ".csv"), row.names = 1)
    data = data[which(data$depth >= Hypolimnion[3,i]),]

    Depth = data$depth
    max.depth = max(Depth)
    Lake.radius = sqrt(Area[i]/pi)
    
    H = max.depth - Depth[1]
    R = H * Lake.radius / max.depth
    Ap = sqrt(H^2 + R^2)
    
    output[i,1] = pi * R * Ap #Lateral area of a cone
    output[i,2] = pi/3 * H * R * R / output[i,1] #Volume of a cone / sediment area
    output[i,3] = H
  }
  return(output)}

  if(!Hypo){
    output = matrix(nrow = dim(Hypolimnion)[2], ncol=2)
    rownames(output) = colnames(Hypolimnion)
    colnames(output) = c("SedArea", "VoltoSedArea")
    
    for(i in 1:length(Area))
    {
      data = read.csv(paste0("../data/Preprocessing/",Year,"/",colnames(Hypolimnion)[i], ".csv"), row.names = 1)
      
      Depth = data$depth
      max.depth = max(Depth)
      if(max.depth == 0) max.depth = 0.5
      Lake.radius = sqrt(Area[i]/pi)
      
      H = max.depth
      R = H * Lake.radius / max.depth
      Ap = sqrt(H^2 + R^2)
      
      output[i,1] = pi * R * Ap #Lateral area of a cone
      output[i,2] = pi/3 * H * R * R / output[i,1] #Volume of a cone / sediment area
    }
    return(output)}
}
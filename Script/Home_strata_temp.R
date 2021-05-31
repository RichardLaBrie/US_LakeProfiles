Home_strata_temp <- function(FileList, Year)
{
  decimalplaces <- function(x) {
    if ((x %% 1) != 0) {
      nchar(strsplit(sub('0+$', '', as.character(x)), ".", fixed=TRUE)[[1]][[2]])
    } else {
      return(0)
    }
  }
  output = matrix(nrow=4, ncol=length(FileList))
  colnames(output) = FileList
  for(i in 1:length(FileList))
  {
    print(i)
    data = read.csv(paste0(dirname(getwd()),"/data/Preprocessing/",Year,"/", FileList[i]))
    filename = unlist(strsplit(FileList[i], ".csv"))
    
    data = data[!is.na(data$temp),]
    
    output[1,i] = min(data$depth)
    output[4,i] = max(data$depth)
    
    #Remove top meter because of surface warming (Yuan and Jones 2020)
    data = data[which(data$depth >= 1),]
    
    #Create a depth profile at every 10 cm or 1cm if the profile ends at .0X
    Zinterval = 10^-decimalplaces(max(data$depth))
    if(Zinterval==1) Zinterval=0.1
    NewDepth = seq(min(data$depth), max(data$depth), Zinterval)
    
    data.linear = approx(x=data$depth, y=data$temp, xout=NewDepth)
    data.linear = as.data.frame(matrix(c(data.linear$x,data.linear$y),ncol = 2))
    colnames(data.linear) = c("depth", "temp")
    
    if(length(data.linear$temp) <=3)
    {
      output[2,i] = -1
      output[3,i] = -1
      colnames(output)[i] <- paste(filename)
    }  else  {
      delta_temp = vector(length=length(data.linear$temp)-1)
      delta_depth = vector(length=length(data.linear$temp)-1)
      for(j in 2:length(data.linear$temp)){
        delta_temp[j-1] = data.linear$temp[j] - data.linear$temp[j-1] #Calculer le delta temperature
        delta_depth[j-1] = data.linear$depth[j] - data.linear$depth[j-1] #Calculer le delta profondeur
      }
      delta_temp_depth = delta_temp/delta_depth*-1 #Calculer les degree/metre en valeur positive
      
      if(max(round(delta_temp_depth,2)) <= 1){
        output[2,i] = -1
        output[3,i] = -1
        colnames(output)[i] <- paste(filename)
        next
        }
      
      #trouver la borne inferieure du meta
      deptheta_up = data.linear$depth[min(which(delta_temp_depth >= 1))] #plus haut dans la colonne d'eau
      #delta_meta_up = delta_temp_depth[min(which(delta_temp_depth > 1))]
      #deptheta_low = data$depth[min(which(delta_temp_depth > 1))+1] #plus bas dans la colonne d'eau
      #delta_meta_low = delta_temp_depth[min(which(delta_temp_depth > 1))+1]
      
      #interpoler la valeur a delta=1
      #if(identical(deptheta_up, numeric(0))) output[2,i] = min(data$depth) else {
      #  output[2,i] = delta_depth[min(which(delta_temp_depth > 1))]/delta_meta_low + deptheta_low }
      output[2,i] = deptheta_up
     
      #trouver la borne inferieure du meta
      # deptheta_up = data$depth[max(which(delta_temp_depth > 1))] #plus haut dans la colonne d'eau
      # delta_meta_up = delta_temp_depth[max(which(delta_temp_depth > 1))]
      deptheta_low = data.linear$depth[max(which(round(delta_temp_depth,2) >= 1))+1] #plus bas dans la colonne d'eau
      # delta_meta_low = delta_temp_depth[max(which(delta_temp_depth > 1))+1]
      
      if(deptheta_low == max(data.linear$depth))
      {
        output[3,i] = -1
      } else {
        #interpoler la valeur a delta=1
        #output[3,i] = delta_depth[max(which(delta_temp_depth > 1))]/delta_meta_low + deptheta_up
        output[3,i] = deptheta_low
      }
      colnames(output)[i] <- paste(filename)
    }
  }
  return(output)
}
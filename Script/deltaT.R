#Metadata contains the sample name in colnames
deltaT <- function(Metadata, Year)
{
  output = vector(length = dim(Metadata)[1])
  names(output) = Metadata$site_id
  for(i in 1:length(output))
  {
    data = read.csv(paste0("../data/Preprocessing/",Year,"/",Metadata$site_id[i], ".csv"), row.names = 1)
    if(any(is.na(data$temp))) data = data[!is.na(data$temp),]
    
    top.temp = data[1,"temp"]
    bot.temp = data[length(data$temp),"temp"]
    
    output[i] = top.temp - bot.temp
  }
  
  return(output)
}

#Debugging
#data = read.csv(paste0("../data/Preprocessing/2007/",Binforest07$site_id[331], ".csv"))
#data = read.csv(paste0("../data/Preprocessing/2012/",Binforest12$site_id[229], ".csv"))
                
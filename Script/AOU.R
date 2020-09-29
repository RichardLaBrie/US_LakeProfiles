#Hypolimnion is a matrix containing the begining and end depths of the hypolimnion
#O2sat is a vector of O2 concentration at saturation
AOU <- function(Hypolimnion, Year, O2sat)
{
  if(dim(Hypolimnion)[2] != length(O2sat)) return(print("Hypolimnion matrix and O2sat length do not match"))
  output = list()
  
  for(i in 1:length(O2sat))
    {
    data = read.csv(paste0("../data/Preprocessing/",Year,"/",colnames(Hypolimnion)[i], ".csv"), row.names = 1)
    # if(any(is.na(data$DO))) {
    #   print(i)
    #   next
    #   }
    if(Year == 2012) if(i %in% c(7, 54, 66, 70, 72, 96, 107, 130, 131, 133, 141, 162, 171))
    {
      data[is.na(data$DO),"DO"] = min(data$DO,na.rm = T)
    }
    DOprofile = data[which(data$depth >= Hypolimnion[3,i]), c("depth", "DO")]
    AOU.temp = O2sat[i] - DOprofile[,2]
    output[[i]] = as.matrix(cbind(depth=DOprofile[,1], AOU.temp))
    names(output)[i] = colnames(Hypolimnion)[i]
    }
  return(output)
}
# data = read.csv(paste0("../data/Preprocessing/2012/",colnames(hypo12)[i], ".csv"), row.names = 1)
# data = read.csv(paste0("../data/Preprocessing/2007/",colnames(hypo07)[i], ".csv"), row.names = 1)

# 2007: i = 39 contains 1 missing value below just thermocline
# 2012: i = 7, 54, 66, 70, 72, 96, 107, 130, 131, 133, 141, 162, 171 contain missing values at the bottom
# 2012: i = 28, 59 contain missing value in epi
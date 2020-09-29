VolumetricAOU = function(AOU.object, Area)
{
  output = vector(length = length(AOU.object))
  names(output) = names(AOU.object)
  for(i in 1:length(AOU.object))
  {
    Depth = AOU.object[[i]][,1]
    AOU = AOU.object[[i]][,2]
    max.depth = max(Depth[which(is.na(AOU)==FALSE)])

    output.temp = vector(length = length(Depth)-1)
	  Lake.radius = sqrt(Area[i]/pi)
	
	  #Calculate for all truncated cone
	  for(j in 1:(length(Depth)-2))
	  {
		  R = (max.depth - Depth[j]) * Lake.radius / max.depth
		  r = (max.depth - Depth[j+1]) * Lake.radius / max.depth
		  h = Depth[j+1] - Depth[j]
		
		  Vol = pi/3 * h * (r*r + r*R + R*R)
		  output.temp[j] = Vol * AOU[j]		
	  }
	
	  #For last strata, we have a full cone
	  Vol = pi/3 * (max.depth - Depth[j+1]) * r*r
	  output.temp[j+1] = Vol * AOU[j+1]
	  output[i] = sum(output.temp)/1000
  }
	return(output)
}